{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SlitherBot.UtilityGrid where

import           ClassyPrelude hiding (toList, tail)
import           Data.Foldable (toList)
import           Control.Lens ((^.))
import           Linear hiding (angle)
import qualified OpenCV as CV
import qualified OpenCV.Unsafe as CV.Unsafe
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits
import           Control.Monad.ST (ST, runST)
import           Linear.V4 (V4)
import qualified Data.HashMap.Strict as HMS
import           Data.Bits ((.|.))
import qualified Data.Sequence as Seq
import           Data.List (iterate, tail)

import           SlitherBot.Protocol
import           SlitherBot.GameState

type UgiRes = 256

ugiRes :: Int32
ugiRes = fromIntegral (natVal (Proxy :: Proxy UgiRes))

ugiSize :: Double
ugiSize = 2000

utilityGridLookup :: UtilityGrid -> V2 Int32 -> Double
utilityGridLookup ug ix = runST $ do
  mug <- CV.Unsafe.unsafeThaw ug
  CV.Unsafe.unsafeRead mug (map fromIntegral (reverse (toList ix)))

-- Length: ugiEdge * ugiEdge
type UtilityGrid      = CV.Mat    (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Float)
type MutUtilityGrid s = CV.MutMat (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Float) s

emptyUtilityGrid :: CV.CvExceptT (ST s) (MutUtilityGrid s)
emptyUtilityGrid = do
  CV.mkMatM
    (Proxy :: Proxy '[UgiRes, UgiRes])
    (Proxy :: Proxy 1)
    (Proxy :: Proxy Float)
    (pure 0 :: V4 Double)

snakeBodyPartRadius :: Double
snakeBodyPartRadius = 70

snakeBodyPredictionIncrease :: Double
snakeBodyPredictionIncrease = 1.05

foodRadius :: Double
foodRadius = 20

blurRadius :: Double
blurRadius = 500

snakeBodyPrediction :: Position -> SnakeBody -> [Position]
snakeBodyPrediction snakePosition snakeBody = let
  headPos = snakePosition
  in case Seq.viewl snakeBody of
    Seq.EmptyL -> []
    previousPos Seq.:< _  -> let
      movement = headPos - previousPos
      in take predictedBodyParts (tail (iterate (+ movement) headPos))

predictedBodyParts :: Int
predictedBodyParts = 7

utilityGrid :: SnakeId -> Position -> GameState -> UtilityGrid
utilityGrid ourSnakeId ourPosition GameState{..} =
  CV.exceptError $ do
    snakesAndFood :: UtilityGrid <- CV.createMat $ do
      mutMat <- emptyUtilityGrid
      forM_ gsFoods $ \Food{..} ->  do
        forM_ (gridIndex ourPosition foodPosition) $ \ix -> do
          let foodUtility = (-0.5) - (foodValue / 100)
          CV.circle mutMat
            ix
            (sizeToPixels foodRadius)
            (pure foodUtility :: V4 Double)
            (-1)
            CV.LineType_8
            0
      forM_ (HMS.toList gsSnakes) $ \(snakeId, Snake{..}) -> do
        when (snakeId /= ourSnakeId) $ do
          forM_ (snakePosition : toList snakeBody) $ \pos ->
            forM_ (gridIndex ourPosition pos) $ \ix ->
              CV.circle mutMat
                ix
                (sizeToPixels snakeBodyPartRadius)
                (pure 1 :: V4 Double)
                (-1)
                CV.LineType_8
                0
          forM_ (zip (snakeBodyPrediction snakePosition snakeBody) (tail (iterate (* snakeBodyPredictionIncrease) 1))) $ \(pos, increase) ->
            forM_ (gridIndex ourPosition pos) $ \ix ->
              CV.circle mutMat
                ix
                (sizeToPixels (snakeBodyPartRadius * increase))
                (pure 1 :: V4 Double)
                (-1)
                CV.LineType_8
                0
      return mutMat
    -- .|. 1 makes the kernel size odd
    blurredSnakesAndFood <-
      CV.gaussianBlur (pure (sizeToPixels blurRadius .|. 1) :: V2 Int32) 0 0 snakesAndFood
    return blurredSnakesAndFood

-- From Position to an index in the UtilityGrid
gridIndex :: Position -> Position -> Maybe (V2 Int32)
gridIndex ourPosition pos = do
  let o = ourPosition ^-^ pure (ugiSize / 2)
  let gridPos = (pos ^-^ o) ^* (fromIntegral ugiRes / ugiSize)
  let gridPosIntegral = floor <$> gridPos
  guard (gridPosIntegral ^. _x < ugiRes && gridPosIntegral ^. _y < ugiRes)
  guard (gridPosIntegral ^. _x >= 0 && gridPosIntegral ^. _y >= 0)
  return gridPosIntegral

sizeToPixels :: (Integral a) => Double -> a
sizeToPixels size = round (size * fromIntegral ugiRes / ugiSize)
