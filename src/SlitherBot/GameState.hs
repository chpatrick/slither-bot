{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module SlitherBot.GameState
  ( GameState(..)
  , Position(..)
  , Direction
  , Fam
  , Snake(..)
  , SnakeBody
  , Food
  ) where

import           ClassyPrelude
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import           Data.Int (Int16, Int8)

import           SlitherBot.Protocol

data GameState = GameState
  { gsOurSnake :: !SnakeId
  , gsSnakes :: !(HMS.HashMap SnakeId Snake)
  , gsFood :: !(HMS.HashMap Position Food)
  } deriving (Eq, Show)

data Position = Position
  { posX :: !Int16
  , posY :: !Int16
  } deriving (Eq, Show, Generic)
instance Hashable Position

type Direction = Double

type Fam = Double

data Snake = Snake
  { snakePosition :: !Position
  , snakeDirection :: !Direction
  , snakeFam :: !Fam
  , snakeBody :: !SnakeBody
  } deriving (Eq, Show)

-- TODO change this super slow data structure
type SnakeBody = Seq.Seq Position

type Food = Int8