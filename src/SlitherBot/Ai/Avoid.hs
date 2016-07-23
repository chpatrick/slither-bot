{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Control.Monad.ST (runST)
import           Control.Monad.Except (runExcept)
import           Linear.V4 (V4)
import qualified Data.HashMap.Strict as HMS

import           SlitherBot.Ai
import           SlitherBot.Protocol
import           SlitherBot.GameState

data AvoidAiState = AvoidAiState
  { aasCurrentAngle :: !Double
  } deriving (Eq, Show)

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
type UtilityGrid = CV.Mat (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Double)

utilityGrid :: UtilityGridInfo -> SnakeId -> Snake -> GameState -> UtilityGrid
utilityGrid UtilityGridInfo{..} ourSnakeId Snake{..} GameState{..} = let
  mbErr = runExcept $ CV.createMat $ do
    CV.mkMatM
      (Proxy :: Proxy '[UgiRes, UgiRes])
      (Proxy :: Proxy 1)
      (Proxy :: Proxy Double)
      (pure 0.5 :: V4 Double)
  in case mbErr of
    Left err -> error ("utilityGrid: got OpenCV error: " ++ show err)
    Right x -> x
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
  { aiInitialState = AvoidAiState 0
  , aiUpdate = \gs@GameState{..} aas -> case gsOwnSnake of
      Nothing -> (AiOutput 0 False, aas)
      Just ourSnakeId -> case HMS.lookup ourSnakeId gsSnakes of
        Nothing -> error ("Could not find our snake " ++ show ourSnakeId)
        Just snake -> let
          ug = utilityGrid utilityGridInfo ourSnakeId snake gs
          in (ug `seq` AiOutput 0 False, aas)
  }