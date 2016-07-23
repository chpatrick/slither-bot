{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module SlitherBot.GameState
  ( GameState(..)
  , defaultGameState
  , Snake(..)
  , SnakeBody
  , updateGameState
  ) where

import           ClassyPrelude
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import           Control.Lens ((^.))
import           Linear.V2 (_x, _y, V2(..))

import           SlitherBot.Protocol

data GameState = GameState
  { gsSnakes :: !(HMS.HashMap SnakeId Snake)
  , gsSetup :: !Setup
  , gsOwnSnake :: !(Maybe SnakeId)
  , gsFoods :: !(HMS.HashMap Position Food)
  } deriving (Eq, Show)

defaultGameState :: GameState
defaultGameState = GameState{ gsSnakes = mempty, gsSetup = defaultSetup, gsOwnSnake = Nothing, gsFoods = mempty }

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
          then V2
            (msPosition ^. _x + snakePosition ^. _x)
            (msPosition ^. _y + snakePosition ^. _y)
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
          then V2
            (isPosition ^. _x + snakePosition ^. _x)
            (isPosition ^. _y + snakePosition ^. _y)
          else isPosition
    let snake' = snake
          { snakePosition = absPosition
          , snakeBody = snakeBody'
          }
    return (Just gs{gsSnakes = HMS.insert isSnakeId snake' gsSnakes})
  MTGameOver -> return Nothing
  MTUnhandled _ -> return (Just gs)
  MTAddSnake AddSnake{..} ->
    let
      snake =
        Snake
        { snakePosition = asPosition
        , snakeDirection = asAngle
        , snakeFam = asFam
        , snakeBody = Seq.fromList asBody
        }
      ownSnake = case gsOwnSnake of
        Nothing -> Just asSnakeId
        _ -> gsOwnSnake
    in return (Just gs{gsSnakes = HMS.insert asSnakeId snake gsSnakes, gsOwnSnake = ownSnake})
  MTRemoveSnake RemoveSnake{..} -> return (Just gs{gsSnakes = HMS.delete rsSnakeId gsSnakes})
  MTAddFood AddFood{..} -> return (Just gs{gsFoods = foldr (\food -> HMS.insert (foodPosition food) food) gsFoods afFoods})
  MTRemoveFood RemoveFood{..} -> return (Just gs{gsFoods = HMS.delete rfPosition gsFoods})
  where
    getSnake snakeId = case HMS.lookup snakeId gsSnakes of
      Nothing -> Left ("Could not find snake " ++ show snakeId)
      Just snake -> Right snake
