{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module SlitherBot.Protocol
  ( NewFood(..)
  , LeaderSnake(..)
  , Config(..)
  , SnakeId(..)
  , defaultConfig
  , ServerMessage(..)
  , parseServerMessage
  ) where

import           ClassyPrelude
import           Data.Serialize.Get
import           Data.Word (Word16)
import qualified Data.ByteString.Char8 as BSC8
import           Control.Monad.Loops (whileM)
import           Data.Bits (shiftL, (.|.))
import           Data.Char (chr)

newtype SnakeId = SnakeId {unSnakeId :: Word16}
  deriving (Eq, Show)

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

data Config = Config
  { grd :: !Word
  , sector_size :: !Double
  , sector_count_along_edge :: !Word16
  , spangdv :: !Double
  , nsp1 :: !Double
  , nsp2 :: !Double
  , nsp3 :: !Double
  , mamu :: !Double
  , mamu2 :: !Double
  , cst :: !Double
  , protocol_version :: !Word8
  } deriving (Eq, Ord, Show)

defaultConfig :: Config
defaultConfig = Config
  { grd = 16384
  , sector_size = 480
  , sector_count_along_edge = 130
  , spangdv = 4.8
  , nsp1 = 4.25
  , nsp2 = 0.5
  , nsp3 = 12
  , mamu  = 0.033
  , mamu2 = 0.28
  , cst = 0.43
  , protocol_version = 2
  }

{-
data ServerMessage = ServerMessage
  { smTimeSinceLastMessage :: !Word16
  , smMessageType :: !MessageType
  , smSnakeId :: !Word16
  } deriving (Eq, Show)

data MessageType
  = MTSetup
  | MT
-}

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

dbg :: (Monad m, Show a) => String -> a -> m ()
dbg name val = traceM (name ++ ": " ++ show val)

w8 :: Num a => Get a
w8 = fromIntegral <$> getWord8

w16 :: Num a => Get a
w16 = fromIntegral <$> getWord16be

w24 :: Num a => Get a
w24 = do
  msb <- w8
  lsbs <- w16
  return $ fromIntegral (((msb `shiftL` 16) .|. lsbs) :: Word)

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