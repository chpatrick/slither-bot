{-# LANGUAGE NoImplicitPrelude #-}
module SlitherBot.GameState
  ( GameState(..)
  , Snake(..)
  , SnakeBody
  ) where

import           ClassyPrelude
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq

import           SlitherBot.Protocol

data GameState = GameState
  { gsOurSnake :: !SnakeId
  , gsSnakes :: !(HMS.HashMap SnakeId Snake)
  } deriving (Eq, Show)

data Snake = Snake
  { snakePosition :: !Position
  , snakeDirection :: !Direction
  , snakeFam :: !Fam
  , snakeBody :: !SnakeBody
  } deriving (Eq, Show)

-- TODO change this super slow data structure
type SnakeBody = Seq.Seq Position
