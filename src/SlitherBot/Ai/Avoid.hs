{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SlitherBot.Ai.Avoid
  ( AvoidAiState
  , avoidAi
  ) where

import           ClassyPrelude
import           Control.Lens ((^.))
import           Linear
import qualified OpenCV as CV
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Except (runExceptT, ExceptT(..))
import           Linear.V4 (V4)
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as T
import qualified Lucid.Html5 as Lucid
import           Data.Bits ((.|.))

import           SlitherBot.Ai
import           SlitherBot.Protocol
import           SlitherBot.GameState

data AvoidAiState = AvoidAiState
  { aasCurrentAngle :: !Double
  , aasUtilityGrid :: !UtilityGrid
  }

type UgiRes = 256

ugiRes :: Int32
ugiRes = fromIntegral (natVal (Proxy :: Proxy UgiRes))

data UtilityGridInfo = UtilityGridInfo
  { ugiSize :: !Double
  } deriving (Eq, Show)

utilityGridInfo :: UtilityGridInfo
utilityGridInfo = UtilityGridInfo{ugiSize = 2000}

-- Length: ugiEdge * ugiEdge
type UtilityGrid      = CV.Mat    (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Double)
type MutUtilityGrid s = CV.MutMat (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Double) s

emptyUtilityGrid :: CV.CvExceptT (ST s) (MutUtilityGrid s)
emptyUtilityGrid = do
  CV.mkMatM
    (Proxy :: Proxy '[UgiRes, UgiRes])
    (Proxy :: Proxy 1)
    (Proxy :: Proxy Double)
    (pure 0 :: V4 Double)

snakeBodyPartRadius :: Double
snakeBodyPartRadius = 30

foodRadius :: Double
foodRadius = 20

blurRadius :: Double
blurRadius = 500

utilityGrid :: UtilityGridInfo -> SnakeId -> Snake -> GameState -> UtilityGrid
utilityGrid UtilityGridInfo{..} ourSnakeId ourSnake GameState{..} =
  CV.exceptError $ do
    snakesAndFood :: UtilityGrid <- CV.createMat $ do
      mutMat <- emptyUtilityGrid
      forM_ (HMS.toList gsSnakes) $ \(snakeId, Snake{..}) -> do
        when (snakeId /= ourSnakeId) $
          forM_ (snakePosition : toList snakeBody) $ \pos ->
            forM_ (gridIndex pos) $ \ix ->
              CV.circle mutMat
                ix
                (sizeToPixels snakeBodyPartRadius)
                (pure 1 :: V4 Double)
                (-1)
                CV.LineType_8
                0
      forM_ gsFoods $ \Food{..} ->  do
        forM_ (gridIndex foodPosition) $ \ix -> do
          let foodUtility = (-0.5) - (foodValue / 100)
          CV.circle mutMat
            ix
            (sizeToPixels foodRadius)
            (pure foodUtility :: V4 Double)
            (-1)
            CV.LineType_8
            0
      return mutMat
    -- .|. 1 makes the kernel size odd
    blurredSnakesAndFood <-
      CV.gaussianBlur (pure (sizeToPixels blurRadius .|. 1) :: V2 Int32) 0 0 snakesAndFood
    {-
    -- We make sure that snake bodies are super bad
    return (CV.matMax blurredSnakesAndFood snakes)
    -}
    return blurredSnakesAndFood
  where
    -- From Position to an index in the UtilityGrid
    gridIndex :: Position -> Maybe (V2 Int32)
    gridIndex pos = do
      let o = snakePosition ourSnake ^-^ pure (ugiSize / 2)
      let gridPos = (pos ^-^ o) ^* (fromIntegral ugiRes / ugiSize)
      let gridPosIntegral = floor <$> gridPos
      guard (gridPosIntegral ^. _x < ugiRes && gridPosIntegral ^. _y < ugiRes)
      guard (gridPosIntegral ^. _x >= 0 && gridPosIntegral ^. _y >= 0)
      return gridPosIntegral

    sizeToPixels :: (Integral a) => Double -> a
    sizeToPixels size = round (size * fromIntegral ugiRes / ugiSize)

avoidAi :: Ai AvoidAiState
avoidAi = Ai
  { aiInitialState = AvoidAiState 0 (CV.exceptError (CV.createMat emptyUtilityGrid))
  , aiUpdate = \gs@GameState{..} aas -> case gsOwnSnake of
      Nothing -> (AiOutput 0 False, aas)
      Just ourSnakeId -> case HMS.lookup ourSnakeId gsSnakes of
        Nothing -> error ("Could not find our snake " ++ show ourSnakeId)
        Just snake -> let
          ug = utilityGrid utilityGridInfo ourSnakeId snake gs
          in (AiOutput 0 False, aas{aasUtilityGrid = ug})
  , aiHtmlStatus = \AvoidAiState{..} -> do
      Lucid.p_ (fromString (show aasCurrentAngle))
      let encodedImg =
            CV.exceptError $ do
              colorGrid :: UtilityGrid <- CV.normalize 0 255 CV.Norm_MinMax Nothing aasUtilityGrid
              CV.imencode (CV.OutputPng CV.defaultPngParams{CV.pngParamCompression = 0}) colorGrid
      Lucid.img_
        [ Lucid.alt_ "Utility grid"
        , Lucid.src_ ("data:image/png;base64," <> T.decodeUtf8 (Base64.encode encodedImg))
        ]
  }
