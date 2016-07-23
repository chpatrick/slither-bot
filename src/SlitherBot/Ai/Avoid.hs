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
import           Control.Monad.ST (ST)
import           Control.Monad.Except (runExcept)
import           Linear.V4 (V4)
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as T
import qualified Lucid.Html5 as Lucid

import           SlitherBot.Ai
import           SlitherBot.Protocol
import           SlitherBot.GameState

data AvoidAiState = AvoidAiState
  { aasCurrentAngle :: !Double
  , aasUtilityGrid :: !UtilityGrid
  }

type UgiRes = 256

ugiRes :: Int
ugiRes = fromIntegral (natVal (Proxy :: Proxy UgiRes))

data UtilityGridInfo = UtilityGridInfo
  { ugiSize :: !Double
  } deriving (Eq, Show)

utilityGridInfo :: UtilityGridInfo
utilityGridInfo = UtilityGridInfo{ugiSize = 500}

type Utility = Double

-- Length: ugiEdge * ugiEdge
type UtilityGrid      = CV.Mat    (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Double)
type MutUtilityGrid s = CV.MutMat (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Double) s

emptyUtilityGrid :: CV.CvExceptT (ST s) (MutUtilityGrid s)
emptyUtilityGrid = do
  CV.mkMatM
    (Proxy :: Proxy '[UgiRes, UgiRes])
    (Proxy :: Proxy 1)
    (Proxy :: Proxy Double)
    (pure 128 :: V4 Double)

utilityGrid :: UtilityGridInfo -> SnakeId -> Snake -> GameState -> UtilityGrid
utilityGrid UtilityGridInfo{..} ourSnakeId Snake{..} GameState{..} =
  CV.exceptError (CV.createMat emptyUtilityGrid)
  where
    -- From Position to an index in the UtilityGrid
    gridIndex :: Position -> Maybe Int 
    gridIndex pos = do
      let o = snakePosition ^-^ pure (ugiSize / 2)
      let gridPos = (pos ^-^ o) ^* (fromIntegral ugiRes / ugiSize)
      let gridPosIntegral = floor <$> gridPos
      guard (gridPosIntegral ^. _x < ugiRes && gridPosIntegral ^. _y < ugiRes)
      guard (gridPosIntegral ^. _x >= 0 && gridPosIntegral ^. _y >= 0)
      return (gridPosIntegral ^. _y * ugiRes + gridPosIntegral ^. _x)

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
            CV.exceptError (CV.imencode (CV.OutputPng CV.defaultPngParams{CV.pngParamCompression = 0}) aasUtilityGrid)
      Lucid.img_
        [ Lucid.alt_ "Utility grid"
        , Lucid.src_ ("data:image/png;base64," <> T.decodeUtf8 (Base64.encode encodedImg))
        ]
  }