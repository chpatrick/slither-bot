{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module SlitherBot.Proxy (proxy) where

import qualified Network.WebSockets  as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http
import qualified Lucid
import qualified Criterion.Measurement
import qualified Data.Text.Encoding as T

import           SlitherBot.Ai
import           SlitherBot.Ai.Search
import           SlitherBot.Protocol
import           SlitherBot.GameState
import           SlitherBot.Prelude

type AiState = SearchAiState

-- Path:
proxy :: forall m. (MonadSlither m) => Int -> m ()
proxy serverPort = do
  $logInfo ("Starting proxy server on port " ++ tshow serverPort)
  run <- askRunBase
  -- Ref where the ai state is stored
  aiStateRef :: IORef (Maybe AiState) <- newIORef Nothing
  liftIO $ Warp.run serverPort $ WS.websocketsOr WS.defaultConnectionOptions
    (run . wsApp aiStateRef) (backupApp aiStateRef)
  where
    ai = searchAi

    wsApp aiStateRef pendingConn = do
      -- Check that nobody else is running
      bracket
        (modifyIORef' aiStateRef $ \mbState -> case mbState of
          Just _ -> error "Another AI already running!"
          Nothing -> Just (aiInitialState ai))
        (\() -> writeIORef aiStateRef Nothing) $ \() -> do
          -- Munge proxy request...
          let reqHead = WS.pendingRequest pendingConn
          host0 <- case unpack . T.decodeUtf8 <$> lookup "Host" (WS.requestHeaders reqHead) of
            Nothing -> fail "Could not find Host header"
            Just host0 -> return host0
          (host, portString) <- case break (== ':') host0 of
            (host, ':' : portString) -> return (host, portString)
            _ -> fail "Could not parse host in host and port"
          let path = unpack (T.decodeUtf8 (WS.requestPath reqHead))
          port <- case readMay portString of
            Nothing -> fail ("Could not read port from " ++ show portString)
            Just port -> return port
          -- strip the Sec-WebSocket-Key header
          let headers = filter (\( header, _ ) -> header /= "Sec-WebSocket-Key") (WS.requestHeaders reqHead)
          -- Get client connection
          clientConn <- liftIO (WS.acceptRequest pendingConn)
          -- Shared game state
          gameStateVar <- newMVar defaultGameState
          -- Connect to remote server
          liftIO Criterion.Measurement.initializeTime
          run <- askRunBase
          liftIO $ WS.runClientWith host port path WS.defaultConnectionOptions headers $ \serverConn -> run $ do
            let
              aiLoop mbPrevOutput = do
                t0 <- liftIO Criterion.Measurement.getTime
                gameState <- readMVar gameStateVar
                $logDebug "Running AI"
                output <- atomicModifyIORef' aiStateRef $ \mbState -> case mbState of
                  Nothing -> error "Could not find AI state!"
                  Just state -> let
                    (output, nextState) = aiUpdate ai gameState state
                    in (Just nextState, output)
                when ((aoAngle <$> mbPrevOutput) /= Just (aoAngle output)) $
                  liftIO (WS.sendBinaryData serverConn (serializeClientMessage (SetAngle (aoAngle output))))
                let speedMsg = if aoSpeedup output
                      then EnterSpeed
                      else LeaveSpeed
                liftIO (WS.sendBinaryData serverConn (serializeClientMessage speedMsg))
                t1 <- liftIO Criterion.Measurement.getTime
                $logInfo ("AI took " ++ tshow (t1 - t0) ++ " seconds")
                liftIO (threadDelay (250 * 1000 - round (max (t1 - t0) 0 * 1000000)))
                aiLoop (Just output)
              clientToServer = do
                -- Foward the first message before starting AI
                firstMessageBs <- liftIO (WS.receiveData clientConn)
                liftIO (WS.sendBinaryData serverConn (firstMessageBs :: ByteString))
                -- Start client to server proxy and AI
                let
                  clientServerLoop = forever $ do
                    $logDebug "Proxying data from client to server"
                    liftIO $ do
                      messageBs :: ByteString <- WS.receiveData clientConn
                      WS.sendBinaryData serverConn messageBs
                fmap (either id id) (race (aiLoop Nothing) clientServerLoop)
              serverToClient = forever $ do
                msg <- liftIO (WS.receiveData serverConn)
                liftIO (WS.sendBinaryData clientConn msg)
                modifyMVar_ gameStateVar $ \gameState -> do
                  case parseServerMessage msg of
                    Left err -> do
                      $logWarn ("Proxying from server to client: couldn't parse " ++ tshow msg ++ ": " ++ pack err)
                      return gameState
                    Right serverMsg -> do
                      $logDebug ("Proxying from server to client: " ++ tshow serverMsg)
                      case updateGameState gameState serverMsg of
                        Left err -> do
                          $logError ("Couldn't update game state: " ++ pack err)
                          return gameState
                        Right Nothing -> return gameState
                        Right (Just gameState') -> return gameState'
            fmap (either id id) (race clientToServer serverToClient)

    backupApp aiStateRef _req cont = do
      mbAiState <- readIORef aiStateRef
      let statusHtml = do
            Lucid.html_ $ do
              Lucid.body_ $ do
                mapM_ (aiHtmlStatus ai) mbAiState
                Lucid.script_ "setTimeout(function() { location.reload(); }, 250)"
      cont (Wai.responseLBS Http.status200 [] (Lucid.renderBS statusHtml))
