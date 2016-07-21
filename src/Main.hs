--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    ( proxy
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad       (forever, unless)
import           Control.Monad.Loops
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Bits
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Builder
import           Data.Char
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Text.Encoding
import           Data.Serialize.Get
import           Data.Word
import           Debug.Trace
import           Network.URI
import qualified Network.WebSockets  as WS
import           System.Random

data NewFood = NewFood
    { newFoodID :: Double -- ID?
    , newFoodX :: Double -- X coordinate
    , newFoodY :: Double -- Y coordinate
    , newFoodQ :: Double -- radius?
    , newFoodColor :: Word8 -- color?
    , newFoodSX :: Word16 -- sector X
    , newFoodSY :: Word16 -- sector Y
    } deriving (Eq, Show)

data LeaderSnake = LeaderSnake
  { leaderName :: BSC.ByteString
  } deriving (Eq, Show)

data Config = Config
  { grd :: Word
  , sector_size :: Double
  , sector_count_along_edge :: Word16
  , spangdv :: Double
  , nsp1 :: Double
  , nsp2 :: Double
  , nsp3 :: Double
  , mamu :: Double
  , mamu2 :: Double
  , cst :: Double
  , protocol_version :: Word8
  } deriving (Eq, Ord, Show)

data ServerMessage
  = Setup Config
  | NewFoods [ NewFood ]
  | GameOver
  | Stats
    { statsRank :: Word16
    , statsSnakeCount :: Word16
    , statsLeaderboard :: [ LeaderSnake ]
    }
  | OtherMessage 
    { msgC :: Word16
    , msgHeader :: Char
    , msgBody :: BSC.ByteString
    }
  deriving (Eq, Show)

whileRemaining :: Get a -> Get [ a ]
whileRemaining = whileM ((>0) <$> remaining)

dbg name val = traceM (name ++ ": " ++ show val)

w8 :: Num a => Get a
w8 = fromIntegral <$> getWord8

w16 :: Num a => Get a
w16 = fromIntegral <$> getWord16be

w24 :: Num a => Get a
w24 = do
  msb <- w8
  lsbs <- w16
  return $ fromIntegral (((msb `shiftL` 16) .|. lsbs) :: Word)

oldProtocol :: Get a
oldProtocol = fail "Unsupported protocol version."

getServerMessage :: Config -> Get ServerMessage
getServerMessage Config{..} = do
  msgC <- getWord16be
  msgHeader <- chr <$> w8
  case msgHeader of
    'a' -> do
      grd <- w24
      e <- w16
      dbg "e" e
      sector_size <- w16
      sector_count_along_edge <- w16
      spangdv <- (/10) <$> w8
      nsp1 <- (/100) <$> w16
      nsp2 <- (/100) <$> w16
      nsp3 <- (/100) <$> w16
      mamu <- (/1E3) <$> w16
      mamu2 <- (/1E3) <$> w16
      cst <- (/1E3) <$> w16
      left <- remaining
      protocol_version <-
        if left > 0
          then w8
          else return $ case defaultConfig of
            Config { protocol_version = defaultVersion } -> defaultVersion
      return $ Setup Config {..}
    'F' -> do -- new foods
      if 4 <= protocol_version
        then
          fmap NewFoods $ whileRemaining $ do
            newFoodColor <- getWord8
            newFoodX <- w16
            newFoodY <- w16
            newFoodQ <- (/5) <$> w8
            let newFoodID = newFoodY * fromIntegral grd * 3 + newFoodX
            let newFoodSX = floor (newFoodX / sector_size)
            let newFoodSY = floor (newFoodY / sector_size)
            return NewFood{..}
        else oldProtocol
    'l' -> do -- leaderboard
      h <- getWord8
      dbg "h" h
      statsRank <- w16
      statsSnakeCount <- w16
      statsLeaderboard <- whileRemaining $ do
        k <- w16
        dbg "k" k
        u <- (\w -> scaleFloat (-24) (fromIntegral w)) <$> w24
        dbg "u" (u :: Double)
        y <- (`mod`9) <$> getWord8
        dbg "y" y
        nameLength <- getWord8
        name <- getByteString (fromIntegral nameLength)
        return (LeaderSnake name)
      bytesLeft <- remaining
      msgBody <- getBytes bytesLeft
      return Stats { .. }
    'v' -> do -- game over
      unknown <- getWord8
      dbg "mystery code" unknown
      return GameOver
    h | h `elem` ("eE345" :: String) -> do
      t <- w16
      e <- remaining
      dbg "h" h
      dbg "e" e
      bytesLeft <- remaining
      msgBody <- getBytes bytesLeft
      return OtherMessage {..}

{-
6 <= protocol_version)
                            6 == e ? (u = "e" == h ? 1 : 2,
                            z = 2 * b[c] * Math.PI / 256,
                            c++,
                            I = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : 5 == e ? "e" == h ? (z = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : "E" == h ? (u = 1,
                            I = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : "4" == h ? (u = 2,
                            I = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : "3" == h ? (u = 1,
                            z = 2 * b[c] * Math.PI / 256,
                            c++,
                            I = 2 * b[c] * Math.PI / 256) : "5" == h && (u = 2,
                            z = 2 * b[c] * Math.PI / 256,
                            c++,
                            I = 2 * b[c] * Math.PI / 256) : 4 == e && ("e" == h ? z = 2 * b[c] * Math.PI / 256 : "E" == h ? (u = 1,
                            I = 2 * b[c] * Math.PI / 256) : "4" == h ? (u = 2,
                            I = 2 * b[c] * Math.PI / 256) : "3" == h && (M = b[c] / 18));
-}
    _ -> do
      bytesLeft <- remaining
      msgBody <- getBytes bytesLeft
      return OtherMessage {..}

instance WS.WebSocketsData Builder where
  fromLazyByteString = lazyByteString
  toLazyByteString = toLazyByteString

login :: Text -> WS.Connection -> IO ()
login nick conn = do
  rnd <- getStdRandom $ randomR ( 0, 8 )
  let loginMsg = word8 115 <> word8 7 <> word8 rnd <> encodeUtf8Builder nick
  WS.sendBinaryData conn loginMsg

{-
app :: WS.ClientApp ()
app conn = do
    putStrLn "Logging in..."
    
    login "kukac" conn

    -- Fork a thread that writes WS data to stdout
    forever $ do
        msg <- WS.receiveData conn
        case runGet getServerMessage msg of
          Left err -> putStrLn $ "Couldn't parse " ++ show msg
          Right msg -> print msg

--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClientWith "176.9.16.170" 446 "/slither" opts headers app
  where
    opts = WS.defaultConnectionOptions
    headers =
      [ ( "Pragma", "no-cache" )
      , ( "Cache-Control", "no-cache" )
      , ( "Origin", "http://slither.io" )
      , ( "Sec-WebSocket-Version", "13" )
      , ( "User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.84 Safari/537.36" )
      , ( "Accept-Encoding", "gzip, deflate, sdch" )
      , ( "Accept-Language", "en-US,en;q=0.8,hu;q=0.6" )
      , ( "Sec-WebSocket-Extensions", "permessage-deflate; client_max_window_bits" )
      ]
-}

defaultConfig :: Config
defaultConfig = Config
  { grd = 16384
  , sector_size = 480
  , sector_count_along_edge = 130
  , spangdv = 4.8
  , nsp1 = 4.25
  , nsp2 = 0.5
  , nsp3 = 12
  , mamu  = 0.033
  , mamu2 = 0.28
  , cst = 0.43
  , protocol_version = 2
  }

proxy :: IO ()
proxy = WS.runServer "127.0.0.1" 1337 $ \pendingConn -> do
  let reqHead = WS.pendingRequest pendingConn
  case parseURI (BSC.unpack (WS.requestPath reqHead)) of
    Just URI { uriPath = path, uriAuthority = Just URIAuth { uriRegName = host, uriPort = ':' : portString } } -> do
      putStrLn "Accepting request"
      print reqHead

      clientConn <- WS.acceptRequest pendingConn
      let port = read portString

      -- strip the Sec-WebSocket-Key header
      let headers = filter (\( header, _ ) -> header /= "Sec-WebSocket-Key") $ WS.requestHeaders reqHead

      configVar <- newMVar defaultConfig

      -- make the equivalent connection to the server
      WS.runClientWith host port path WS.defaultConnectionOptions headers $ \serverConn -> do
        -- proxy messages from the client to the server
        clientToServer <- async $ forever $ do
          msg <- WS.receiveData clientConn
          WS.sendBinaryData serverConn (msg :: BSC.ByteString)

        -- proxy messages from the server to the client
        serverToClient <- async $ forever $ do
          msg <- WS.receiveData serverConn
          WS.sendBinaryData clientConn msg
          modifyMVar_ configVar $ \cfg -> do
            case runGet (getServerMessage cfg) msg of
              Left err -> do
                putStrLn $ "Couldn't parse " ++ show msg
                return cfg
              Right msg -> do
                print msg
                return $ case msg of
                  Setup newCfg -> newCfg
                  _ -> cfg

        _ <- waitAnyCatchCancel [ serverToClient, clientToServer ]
        putStrLn "Connection ended."
        return ()

    Nothing -> WS.rejectRequest pendingConn ""