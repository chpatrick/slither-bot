{-# LANGUAGE RecordWildCards #-}
module SlitherBot.Ai
  ( AiOutput(..)
  , Ai(..)
  ) where

import qualified Lucid

import           SlitherBot.GameState

data AiOutput = AiOutput
  { aoAngle :: !Double
  , aoSpeedup :: !Bool
  } deriving (Eq, Show)

data Ai s = Ai
  { aiUpdate :: !(GameState -> s -> (AiOutput, s))
  , aiInitialState :: !s
  , aiHtmlStatus :: !(s -> Lucid.Html ())
  }
