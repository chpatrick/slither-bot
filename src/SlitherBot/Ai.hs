{-# LANGUAGE RecordWildCards #-}
module SlitherBot.Ai
  ( calculateNextMove
  , AiState(..)
  ) where


import           SlitherBot.GameState
import           SlitherBot.Protocol

import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import           Data.Fixed

data AiState
  = AiState
    { aiStateAngle :: Double
    }

calculateNextMove :: GameState -> AiState -> (Double, AiState)
calculateNextMove GameState{..} as@AiState{..} =
  let newAngle = (aiStateAngle + pi / 4) `mod'` (2 * pi)
  in (newAngle, as { aiStateAngle = newAngle })

  -- Nothing -> (0.0, as)
  -- Just ownId -> case HMS.lookup ownId gsSnakes of
  --   Nothing -> error ("Cannot find own snake ID! (" ++ show ownId ++ ")")
  --   Just _snake ->
  --     in

-- getAngle :: Snake -> Double
-- getAngle Snake{..} = case Seq.viewl snakeBody of
--   Seq.EmptyL -> 0.0
--   firstBodyPart Seq.:< _ -> case (snakePosition, firstBodyPart) of
--     (Position x0 y0, Position x1 y1) -> atan2 (y0 - y1) (x0 - x1)
