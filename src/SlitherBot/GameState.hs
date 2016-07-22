{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module SlitherBot.GameState
  ( GameState(..)
  , Snake(..)
  , SnakeBody
  , updateGameState
  ) where

import           ClassyPrelude
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq

import           SlitherBot.Protocol

data GameState = GameState
  { gsOurSnake :: !SnakeId
  , gsSnakes :: !(HMS.HashMap SnakeId Snake)
  , gsSetup :: !Setup
  } deriving (Eq, Show)

data Snake = Snake
  { snakePosition :: !Position
  , snakeDirection :: !Direction
  , snakeFam :: !Fam
  , snakeBody :: !SnakeBody
  } deriving (Eq, Show)

-- TODO change this super slow data structure
type SnakeBody = Seq.Seq Position

updateGameState :: GameState -> ServerMessage -> Either String (Maybe GameState)
updateGameState gs@GameState{..} ServerMessage{..} = case smMessageType of
  MTSetup setup -> Right (Just gs{gsSetup = setup})
  MTRemoveLastPart RemoveLastPart{..} -> do
    snake@Snake{..} <- getSnake rlpSnakeId
    case Seq.viewr snakeBody of
      Seq.EmptyR -> Left ("Cannot remove last part of empty snake " ++ show rlpSnakeId)
      snakeBody' Seq.:> _ -> return (Just gs{gsSnakes = HMS.insert rlpSnakeId snake{snakeBody = snakeBody'} gsSnakes})
  MTMoveSnake MoveSnake{..} -> do
    snake@Snake{..} <- getSnake msSnakeId
    let snakeBody1 = case Seq.viewr snakeBody of
          Seq.EmptyR -> snakeBody
          snakeBody' Seq.:> _ -> snakeBody'
    let snakeBody2 = snakePosition Seq.<| snakeBody1
    let absPosition = if msRelative
          then Position
            { posX = posX msPosition - 128 + posX snakePosition
            , posY = posY msPosition - 128 + posY snakePosition
            }
          else msPosition
    let snake' = snake
          { snakePosition = absPosition
          , snakeBody = snakeBody2
          }
    return (Just gs{gsSnakes = HMS.insert msSnakeId snake' gsSnakes})
  MTIncreaseSnake IncreaseSnake{..} -> do
    snake@Snake{..} <- getSnake isSnakeId
    let snakeBody' = snakePosition Seq.<| snakeBody
    let absPosition = if isRelative
          then Position
            { posX = posX isPosition - 128 + posX snakePosition
            , posY = posY isPosition - 128 + posY snakePosition
            }
          else isPosition
    let snake' = snake
          { snakePosition = absPosition
          , snakeBody = snakeBody'
          }
    return (Just gs{gsSnakes = HMS.insert isSnakeId snake' gsSnakes})
  MTGameOver -> return Nothing
  MTUnhandled _ -> return (Just gs)
  where
    getSnake snakeId = case HMS.lookup snakeId gsSnakes of
      Nothing -> Left ("Could not find SnakeId " ++ show snakeId)
      Just snake -> Right snake
