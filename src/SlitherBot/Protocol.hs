{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SlitherBot.Protocol
  ( Setup(..)
  , SnakeId(..)
  , defaultSetup
  , FirstClientMessage(..)
  , ClientMessage(..)
  , parseClientMessage
  , serializeClientMessage
  , ServerMessage(..)
  , parseServerMessage
  , Fam
  , Position
  , MessageType(..)
  , RemoveLastPart(..)
  , Direction
  , MoveSnake(..)
  , IncreaseSnake(..)
  , AddSnake(..)
  , RemoveSnake(..)
  , AddFood(..)
  , RemoveFood(..)
  , Food(..)
  ) where

import           ClassyPrelude
import           Data.Serialize.Get
import           Data.Serialize.Put
import           Data.Word (Word16)
import qualified Data.ByteString.Char8 as BSC8
import           Data.Bits (shiftL, (.|.))
import           Data.Char (chr)
import           Linear.V2 hiding (angle)
import           Control.Lens ((^.))

newtype SnakeId = SnakeId {unSnakeId :: Word16}
  deriving (Eq, Show, Hashable)

type Position = V2 Double

data Setup = Setup
  { setupGrid :: !Int64
  , setupMscps :: !Int64
  , setupSectorSize :: !Int64
  , setupSectorCountAlongEdge :: !Int64
  , setupSpangdv :: !Double
  , setupNsp1 :: !Double
  , setupNsp2 :: !Double
  , setupNsp3 :: !Double
  , setupMamu :: !Double
  , setupMamu2 :: !Double
  , setupCst :: !Double
  , setupProtocol :: !Word8
  } deriving (Eq, Ord, Show)

defaultSetup :: Setup
defaultSetup = Setup
  { setupGrid = 21600
  , setupMscps = 411
  , setupSectorSize = 480
  , setupSectorCountAlongEdge = 130
  , setupSpangdv = 4.8
  , setupNsp1 = 4.25
  , setupNsp2 = 0.5
  , setupNsp3 = 12
  , setupMamu = 0.033
  , setupMamu2 = 0.028
  , setupCst = 0.43
  , setupProtocol = 8
  }

type Fam = Double

-- Clockwise radians from looking north
type Direction = Double

data FirstClientMessage
  = SetUsernameAndSkin
    { suasProtocolVersion :: !Word8
    , suasSkinId :: !Word8
    , suasName :: !ByteString
    }
  deriving (Eq, Show)

data ClientMessage
  = Ping
  | SetAngle !Double
  | Turn !Double
  | EnterSpeed
  | LeaveSpeed
  deriving (Eq, Show)

data ServerMessage = ServerMessage
  { smTimeSinceLastMessage :: !Word16
  , smMessageType :: !MessageType
  } deriving (Eq, Show)

data MessageType
  = MTSetup !Setup
  | MTRemoveLastPart !RemoveLastPart
  | MTMoveSnake !MoveSnake
  | MTIncreaseSnake !IncreaseSnake
  | MTUnhandled !Char
  | MTAddSnake !AddSnake
  | MTRemoveSnake !RemoveSnake
  | MTAddFood !AddFood
  | MTRemoveFood !RemoveFood
  | MTGameOver
  deriving (Eq, Show)

data RemoveLastPart = RemoveLastPart
  { rlpSnakeId :: !SnakeId
  , rlpNewFam :: !(Maybe Fam)
  } deriving (Eq, Show)

data MoveSnake = MoveSnake
  { msSnakeId :: !SnakeId
  , msRelative :: !Bool
  , msPosition :: !Position
  } deriving (Eq, Show)

data IncreaseSnake = IncreaseSnake
  { isSnakeId :: !SnakeId
  , isRelative :: !Bool
  , isPosition :: !Position
  , isNewFam :: !Fam
  } deriving (Eq, Show)

data AddSnake =
  AddSnake
  { asSnakeId :: !SnakeId
  , asEhangWehang :: !Double
  , asAngle :: !Double
  , asSpeed :: !Double
  , asFam :: !Double
  , asSkin :: !Word8
  , asPosition :: !Position
  , asName :: !ByteString
  , asBody :: ![Position]
  }
  deriving (Eq, Show)

data AddFood =
  AddFood
  { afFoods :: ![Food]
  }
  deriving (Eq, Show)

data RemoveFood =
  RemoveFood
  { rfPosition :: !Position
  }
  deriving (Eq, Show)

data Food
  = Food
    { foodColor :: !Word8
    , foodPosition :: !Position
    , foodValue :: !Double
    }
  deriving (Eq, Ord, Show)

data RemoveSnake
  = RemoveSnake
    { rsSnakeId :: !SnakeId
    , rsDied :: !Bool
    }
  deriving (Eq, Show)

i8 :: Num a => Get a
i8 = fromIntegral <$> getInt8

i16 :: Num a => Get a
i16 = fromIntegral <$> getInt16be

i24 :: Num a => Get a
i24 = do
  msb <- i8
  lsbs <- i16
  return $ fromIntegral (((msb `shiftL` 16) .|. lsbs) :: Word)

dbg :: (Monad m, Show a) => String -> a -> m ()
dbg name val = traceM (name ++ ": " ++ show val)

parseServerMessage :: ByteString -> Either String ServerMessage
parseServerMessage input = runGet (getServerMessage (BSC8.length input)) input

parseClientMessage :: ByteString -> Either String ClientMessage
parseClientMessage input = runGet getClientMessage input

getServerMessage :: Int -> Get ServerMessage
getServerMessage inputLength = do
  timeSinceLastMessage <- getWord16be
  messageType <- getMessageType inputLength
  return (ServerMessage timeSinceLastMessage messageType)

unexpectedInputSize :: Monad m => Int -> m a
unexpectedInputSize size = fail ("Unexpected input size " ++ show size)

getSnakeId :: Get SnakeId
getSnakeId = SnakeId <$> i16

getPosition16 :: Get Position
getPosition16 = do
  x <- i16
  y <- i16
  return (V2 x y)

getPosition8 :: Get Position
getPosition8 = do
  x <- i8
  y <- i8
  return (V2 x y)

getPosition5 :: Get Position
getPosition5 = do
  x <- i24
  y <- i24
  return (V2 (x / 5) (y / 5))

_MAGIC_NUMBER :: Double
_MAGIC_NUMBER = 16777215

getFam :: Get Fam
getFam = do
  fam24 <- i24
  return (fam24 / _MAGIC_NUMBER)

getAngle :: Get Double
getAngle = do
  angle24 <- i24
  return (angle24 * 2 * pi / _MAGIC_NUMBER)

getMessageType :: Int -> Get MessageType
getMessageType inputLength = do
  msgHeader <- chr <$> i8
  case msgHeader of
    'a' -> do
      grd <- i24
      e <- i16
      dbg "e" e
      sector_size <- i16
      sector_count_along_edge <- i16
      spangdv <- (/10) <$> i8
      nsp1 <- (/100) <$> i16
      nsp2 <- (/100) <$> i16
      nsp3 <- (/100) <$> i16
      mamu <- (/1E3) <$> i16
      mamu2 <- (/1E3) <$> i16
      cst <- (/1E3) <$> i16
      left <- remaining
      protocol_version <-
        if left > 0
          then i8
          else return $ case defaultSetup of
            Setup { setupProtocol = defaultVersion } -> defaultVersion
      let
        setup =
          Setup
          { setupGrid = grd
          , setupMscps = e
          , setupSectorSize = sector_size
          , setupSectorCountAlongEdge = sector_count_along_edge
          , setupSpangdv = spangdv
          , setupNsp1 = nsp1
          , setupNsp2 = nsp2
          , setupNsp3 = nsp3
          , setupMamu = mamu
          , setupMamu2 = mamu2
          , setupCst = cst
          , setupProtocol = protocol_version
          }
      return (MTSetup setup)
    -- 'F' -> do -- new foods
    --   if 4 <= protocol_version
    --     then
    --       fmap NewFoods $ whileRemaining $ do
    --         newFoodColor <- getWord8
    --         newFoodX <- i16
    --         newFoodY <- i16
    --         newFoodQ <- (/5) <$> i8
    --         let newFoodID = newFoodY * fromIntegral grd * 3 + newFoodX
    --         let newFoodSX = floor (newFoodX / sector_size)
    --         let newFoodSY = floor (newFoodY / sector_size)
    --         return NewFood{..}
    --     else oldProtocol
    -- 'l' -> do -- leaderboard
    --   h <- getWord8
    --   dbg "h" h
    --   statsRank <- i16
    --   statsSnakeCount <- i16
    --   statsLeaderboard <- whileRemaining $ do
    --     k <- i16
    --     dbg "k" k
    --     u <- (\w -> scaleFloat (-24) (fromIntegral w)) <$> i24
    --     dbg "u" (u :: Double)
    --     y <- (`mod`9) <$> getWord8
    --     dbg "y" y
    --     nameLength <- getWord8
    --     name <- getByteString (fromIntegral nameLength)
    --     return (LeaderSnake name)
    --   bytesLeft <- remaining
    --   msgBody <- getBytes bytesLeft
    --   return Stats { .. }
    'r' -> do
      removeLastPart <- case inputLength of
        5 -> do
          snakeId <- getSnakeId
          return (RemoveLastPart snakeId Nothing)
        8 -> do
          snakeId <- getSnakeId
          newFam <- getFam
          return (RemoveLastPart snakeId (Just newFam))
        size -> do
          unexpectedInputSize size
      return (MTRemoveLastPart removeLastPart)
    'g' -> do
      snakeId <- getSnakeId
      position <- getPosition16
      return (MTMoveSnake (MoveSnake snakeId False position))
    'G' -> do
      snakeId <- getSnakeId
      position <- getPosition8
      return (MTMoveSnake (MoveSnake snakeId True position))
    'n' -> do
      snakeId <- getSnakeId
      position <- getPosition16
      newFam <- getFam
      return (MTIncreaseSnake (IncreaseSnake snakeId False position newFam))
    'N' -> do
      snakeId <- getSnakeId
      position <- getPosition8
      newFam <- getFam
      return (MTIncreaseSnake (IncreaseSnake snakeId True position newFam))
    's' -> do
      snakeId <- getSnakeId
      case inputLength of
        6 -> do
          diedByte <- i8
          return (MTRemoveSnake (RemoveSnake snakeId (diedByte == (1 :: Word8))))
        other
          | other >= 31 -> do

              ehangWehang <- getAngle
              _unused <- getWord8
              angle <- getAngle
              speed <- (/ 1e3) <$> i16
              fam <- getFam
              skin <- i8
              position <- getPosition5
              nameLength <- i8
              name <- getBytes nameLength
              tailPart <- getPosition5
              let
                restLength = inputLength - 25 - nameLength - 6
                numberOfParts = restLength `div` 2
              actuallyRemaining <- remaining
              unless (restLength == actuallyRemaining) (fail ("Actually remaining no. of bytes(" ++ show actuallyRemaining ++ ") != expected no. of bytes(" ++ show restLength ++ ")"))
              unless (restLength >= 0) (fail ("Snake body length = " ++ show restLength ++ " < 0"))
              unless (restLength `mod` 2 == 0) (fail ("Snake body length(" ++ show restLength ++ ") `mod` 2 != 0"))
              relativePositions <- replicateM numberOfParts getPosition8
              let
                toGlobalPositions _current acc [] = acc
                toGlobalPositions current acc (relative : rest) =
                  let nextPosition = V2 (current ^. _x + ((relative ^. _x) / 2)) (current ^. _y + ((relative ^. _y) / 2))
                  in toGlobalPositions nextPosition (nextPosition : acc) rest

                addSnake =
                  AddSnake
                  { asSnakeId = snakeId
                  , asEhangWehang = ehangWehang
                  , asAngle = angle
                  , asSpeed = speed
                  , asFam = fam
                  , asSkin = skin
                  , asPosition = position
                  , asName = name
                  , asBody = toGlobalPositions tailPart [tailPart] relativePositions
                  }
              return (MTAddSnake addSnake)
          | otherwise -> do
              unexpectedInputSize other

    'F' -> do
      MTAddFood <$> getAddFood inputLength

    'f' -> do
      MTAddFood <$> getAddFood inputLength

    'b' -> do
      MTAddFood <$> getAddFood inputLength

    'v' -> do -- game over
      unknown <- getWord8
      dbg "mystery code" unknown
      return MTGameOver
    other -> return (MTUnhandled other)

    -- h | h `elem` ("eE345" :: String) -> do
    --   t <- i16
    --   e <- remaining
    --   dbg "h" h
    --   dbg "e" e
    --   bytesLeft <- remaining
    --   msgBody <- getBytes bytesLeft
    --   return OtherMessage {..}

getAddFood :: Int -> Get AddFood
getAddFood inputLength = do
  let restLength = inputLength - 3
  actuallyRemaining <- remaining
  unless (restLength == actuallyRemaining) (fail ("Actually remaining no. of bytes(" ++ show actuallyRemaining ++ ") != expected no. of bytes(" ++ show restLength ++ ")"))
  unless (restLength `mod` 6 /= 0) (fail ("(restLength(" ++ show restLength ++ ") `mod` 6 /= 0) "))
  foods <- replicateM (restLength `div` 6) getFood
  return (AddFood foods)

getFood :: Get Food
getFood = do
  color <- i8
  position <- getPosition16
  valueByte <- i8
  return (Food color position (valueByte / 5))

getClientMessage :: Get ClientMessage
getClientMessage = do
  firstByte <- getWord8
  case firstByte of
    angle | angle <= 250 -> return (SetAngle ((fromIntegral angle / 251) * 2 * pi))
    251 -> return Ping
    252 -> do
      angleByte <- getWord8
      return (Turn ((fromIntegral angleByte / 256) * 2 * pi))
    253 -> return EnterSpeed
    254 -> return LeaveSpeed
    other -> fail ("Unexpected client byte " ++ show other)

putClientMessage :: ClientMessage -> Put
putClientMessage message = case message of
  Ping -> putWord8 251
  SetAngle angle -> putWord8 (floor (angle / (2 * pi) * 251))
  Turn angle -> do
    putWord8 252
    putWord8 (floor (angle / (2 * pi) * 256))
  EnterSpeed -> putWord8 253
  LeaveSpeed -> putWord8 254

serializeClientMessage :: ClientMessage -> ByteString
serializeClientMessage message = runPut (putClientMessage message)
