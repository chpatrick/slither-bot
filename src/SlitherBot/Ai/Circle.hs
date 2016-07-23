{-# LANGUAGE RecordWildCards #-}
module SlitherBot.Ai.Circle
  ( CircleAiState
  , circleAi
  ) where

import           Data.Fixed

import           SlitherBot.Ai
import           SlitherBot.GameState

data CircleAiState = CircleAiState
  { casAngle :: !Double
  } deriving (Eq, Show)

circleAi :: Ai CircleAiState
circleAi = Ai
  { aiUpdate = \GameState{..} cas@CircleAiState{..} ->
      let newAngle = (casAngle + pi / 4) `mod'` (2 * pi)
      in (AiOutput{aoAngle = newAngle, aoSpeedup = False}, cas{ casAngle = newAngle })
  , aiInitialState = CircleAiState{casAngle = 0}
  }
