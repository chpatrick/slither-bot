{-# LANGUAGE RecordWildCards #-}
module SlitherBot.Ai
  ( AiOutput(..)
  , Ai(..)
  ) where

import           SlitherBot.GameState
import           SlitherBot.Protocol

import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import           Data.Fixed

data AiOutput = AiOutput
  { aoAngle :: !Double
  , aoSpeedup :: !Bool
  } deriving (Eq, Show)

data Ai s = Ai
  { aiUpdate :: !(GameState -> s -> (AiOutput, s))
  , aiInitialState :: !s
  }
