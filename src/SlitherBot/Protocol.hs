{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SlitherBot.Protocol
  ( Setup(..)
  , SnakeId(..)
  , defaultSetup
  , ServerMessage(..)
  , parseServerMessage
  , Fam
  , Position(..)
  , MessageType(..)
  , RemoveLastPart(..)
  , Direction
  , MoveSnake(..)
  , IncreaseSnake(..)
  , RemoveSnake(..)
  ) where

import           ClassyPrelude
import           Data.Serialize.Get
import           Data.Word (Word16)
import qualified Data.ByteString.Char8 as BSC8
import           Data.Bits (shiftL, (.|.))
import           Data.Char (chr)

newtype SnakeId = SnakeId {unSnakeId :: Word16}
  deriving (Eq, Show, Hashable)

data Position = Position
  { posX :: !Double
  , posY :: !Double
  } deriving (Eq, Show, Generic)
instance Hashable Position

{-
data NewFood = NewFood
    { newFoodID :: !Double -- ID?
    , newFoodX :: !Double -- X coordinate
    , newFoodY :: !Double -- Y coordinate
    , newFoodQ :: !Double -- radius?
    , newFoodColor :: !Word8 -- color?
    , newFoodSX :: !Word16 -- sector X
    , newFoodSY :: !Word16 -- sector Y
    } deriving (Eq, Show)

data LeaderSnake = LeaderSnake
  { leaderName :: !BSC8.ByteString
  } deriving (Eq, Show)
-}

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

data RemoveSnake
  = RemoveSnake
    { rsSnakeId :: !SnakeId
    , rsDied :: !Bool
    }
  deriving (Eq, Show)

{-
data ServerMessage
  = Setup Config
  | NewFoods [ NewFood ]
  | GameOver
  | Stats
    { statsRank :: !Word16
    , statsSnakeCount :: !Word16
    , statsLeaderboard :: ![ LeaderSnake ]
    }
  | OtherMessage
    { msgC :: !Word16
    , msgHeader :: !Char
    , msgBody :: !BSC8.ByteString
    }
  deriving (Eq, Show)

whileRemaining :: Get a -> Get [ a ]
whileRemaining = whileM ((>0) <$> remaining)

oldProtocol :: Get a
oldProtocol = fail "Unsupported protocol version."

getServerMessage :: Config -> Get ServerMessage
getServerMessage Config{..} = do
  msgC <- getWord16be
  msgHeader <- chr <$> w8
  case msgHeader of
    'a' -> do
      grd <- w24
      e <- w16
      dbg "e" e
      sector_size <- w16
      sector_count_along_edge <- w16
      spangdv <- (/10) <$> w8
      nsp1 <- (/100) <$> w16
      nsp2 <- (/100) <$> w16
      nsp3 <- (/100) <$> w16
      mamu <- (/1E3) <$> w16
      mamu2 <- (/1E3) <$> w16
      cst <- (/1E3) <$> w16
      left <- remaining
      protocol_version <-
        if left > 0
          then w8
          else return $ case defaultConfig of
            Config { protocol_version = defaultVersion } -> defaultVersion
      return $ Setup Config {..}
    'F' -> do -- new foods
      if 4 <= protocol_version
        then
          fmap NewFoods $ whileRemaining $ do
            newFoodColor <- getWord8
            newFoodX <- w16
            newFoodY <- w16
            newFoodQ <- (/5) <$> w8
            let newFoodID = newFoodY * fromIntegral grd * 3 + newFoodX
            let newFoodSX = floor (newFoodX / sector_size)
            let newFoodSY = floor (newFoodY / sector_size)
            return NewFood{..}
        else oldProtocol
    'l' -> do -- leaderboard
      h <- getWord8
      dbg "h" h
      statsRank <- w16
      statsSnakeCount <- w16
      statsLeaderboard <- whileRemaining $ do
        k <- w16
        dbg "k" k
        u <- (\w -> scaleFloat (-24) (fromIntegral w)) <$> w24
        dbg "u" (u :: Double)
        y <- (`mod`9) <$> getWord8
        dbg "y" y
        nameLength <- getWord8
        name <- getByteString (fromIntegral nameLength)
        return (LeaderSnake name)
      bytesLeft <- remaining
      msgBody <- getBytes bytesLeft
      return Stats { .. }
    'v' -> do -- game over
      unknown <- getWord8
      dbg "mystery code" unknown
      return GameOver
    h | h `elem` ("eE345" :: String) -> do
      t <- w16
      e <- remaining
      dbg "h" h
      dbg "e" e
      bytesLeft <- remaining
      msgBody <- getBytes bytesLeft
      return OtherMessage {..}

{-
6 <= protocol_version)
                            6 == e ? (u = "e" == h ? 1 : 2,
                            z = 2 * b[c] * Math.PI / 256,
                            c++,
                            I = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : 5 == e ? "e" == h ? (z = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : "E" == h ? (u = 1,
                            I = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : "4" == h ? (u = 2,
                            I = 2 * b[c] * Math.PI / 256,
                            c++,
                            M = b[c] / 18) : "3" == h ? (u = 1,
                            z = 2 * b[c] * Math.PI / 256,
                            c++,
                            I = 2 * b[c] * Math.PI / 256) : "5" == h && (u = 2,
                            z = 2 * b[c] * Math.PI / 256,
                            c++,
                            I = 2 * b[c] * Math.PI / 256) : 4 == e && ("e" == h ? z = 2 * b[c] * Math.PI / 256 : "E" == h ? (u = 1,
                            I = 2 * b[c] * Math.PI / 256) : "4" == h ? (u = 2,
                            I = 2 * b[c] * Math.PI / 256) : "3" == h && (M = b[c] / 18));
-}
    _ -> do
      bytesLeft <- remaining
      msgBody <- getBytes bytesLeft
      return OtherMessage {..}

parseServerMessage :: Config -> ByteString -> Either String ServerMessage
parseServerMessage cfg = runGet (getServerMessage cfg)
-}

w8 :: Num a => Get a
w8 = fromIntegral <$> getWord8

w16 :: Num a => Get a
w16 = fromIntegral <$> getWord16be

w24 :: Num a => Get a
w24 = do
  msb <- w8
  lsbs <- w16
  return $ fromIntegral (((msb `shiftL` 16) .|. lsbs) :: Word)

dbg :: (Monad m, Show a) => String -> a -> m ()
dbg name val = traceM (name ++ ": " ++ show val)

parseServerMessage :: Setup -> ByteString -> Either String ServerMessage
parseServerMessage setup input = runGet (getServerMessage (BSC8.length input) setup) input

getServerMessage :: Int -> Setup -> Get ServerMessage
getServerMessage inputLength _ = do
  timeSinceLastMessage <- getWord16be
  messageType <- getMessageType inputLength
  return (ServerMessage timeSinceLastMessage messageType)

unexpectedInputSize :: Monad m => Int -> m a
unexpectedInputSize size = fail ("Unexpected input size " ++ show size)

getSnakeId :: Get SnakeId
getSnakeId = SnakeId <$> w16

getPosition16 :: Get Position
getPosition16 = do
  x <- w16
  y <- w16
  return (Position x y)

getPosition8 :: Get Position
getPosition8 = do
  x <- w8
  y <- w8
  return (Position x y)

getPosition5 :: Get Position
getPosition5 = do
  x <- w24
  y <- w24
  return (Position (x / 5) (y / 5))

_MAGIC_NUMBER :: Double
_MAGIC_NUMBER = 16777215

getFam :: Get Fam
getFam = do
  fam24 <- w24
  return (fam24 / _MAGIC_NUMBER)

getAngle :: Get Double
getAngle = do
  angle24 <- w24
  return (angle24 * 2 * pi / _MAGIC_NUMBER)

getMessageType :: Int -> Get MessageType
getMessageType inputLength = do
  msgHeader <- chr <$> w8
  case msgHeader of
    'a' -> do
      grd <- w24
      e <- w16
      dbg "e" e
      sector_size <- w16
      sector_count_along_edge <- w16
      spangdv <- (/10) <$> w8
      nsp1 <- (/100) <$> w16
      nsp2 <- (/100) <$> w16
      nsp3 <- (/100) <$> w16
      mamu <- (/1E3) <$> w16
      mamu2 <- (/1E3) <$> w16
      cst <- (/1E3) <$> w16
      left <- remaining
      protocol_version <-
        if left > 0
          then w8
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
    --         newFoodX <- w16
    --         newFoodY <- w16
    --         newFoodQ <- (/5) <$> w8
    --         let newFoodID = newFoodY * fromIntegral grd * 3 + newFoodX
    --         let newFoodSX = floor (newFoodX / sector_size)
    --         let newFoodSY = floor (newFoodY / sector_size)
    --         return NewFood{..}
    --     else oldProtocol
    -- 'l' -> do -- leaderboard
    --   h <- getWord8
    --   dbg "h" h
    --   statsRank <- w16
    --   statsSnakeCount <- w16
    --   statsLeaderboard <- whileRemaining $ do
    --     k <- w16
    --     dbg "k" k
    --     u <- (\w -> scaleFloat (-24) (fromIntegral w)) <$> w24
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
          diedByte <- w8
          return (MTRemoveSnake (RemoveSnake snakeId (diedByte == (1 :: Word8))))
        other
          | other >= 31 -> do

              ehangWehang <- getAngle
              _unused <- getWord8
              angle <- getAngle
              speed <- (/ 1e3) <$> w16
              fam <- getFam
              skin <- w8
              position <- getPosition5
              nameLength <- w8
              name <- getBytes nameLength
              tailPart <- getPosition5
              let
                restLength = inputLength - 24 - nameLength - 6
                numberOfParts = restLength `div` 2
              unless (restLength >= 0) (fail ("Snake body length = " ++ show restLength ++ " < 0"))
              unless (restLength `mod` 2 == 0) (fail ("Snake body length `mod` 2 = " ++ show restLength ++ " != 0"))
              relativePositions <- replicateM numberOfParts getPosition8
              let
                toGlobalPositions _current acc [] = acc
                toGlobalPositions current acc (relative : rest) =
                  let nextPosition = Position (posX current + ((posX relative - 127) / 2)) (posY current + ((posY relative - 127) / 2))
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
                  , asBody = toGlobalPositions tailPart relativePositions [tailPart]
                  }             --
              return (MTAddSnake addSnake)
          | otherwise -> do
              unexpectedInputSize other
    'v' -> do -- game over
      unknown <- getWord8
      dbg "mystery code" unknown
      return MTGameOver
    other -> return (MTUnhandled other)

    -- h | h `elem` ("eE345" :: String) -> do
    --   t <- w16
    --   e <- remaining
    --   dbg "h" h
    --   dbg "e" e
    --   bytesLeft <- remaining
    --   msgBody <- getBytes bytesLeft
    --   return OtherMessage {..}
