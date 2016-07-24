{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SlitherBot.Ai.Avoid
  ( AvoidAiState
  , avoidAi
  ) where

import           ClassyPrelude hiding (toList)
import           Data.Foldable (toList)
import           Control.Lens ((^.))
import           Linear hiding (angle)
import qualified Linear
import qualified OpenCV as CV
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Except (runExceptT, ExceptT(..))
import           Linear.V4 (V4)
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as T
import qualified Lucid.Html5 as Lucid
import           Data.Bits ((.|.))
import qualified OpenCV.Unsafe as CV.Unsafe
import           Data.Fixed (mod')

import           SlitherBot.Ai
import           SlitherBot.Protocol
import           SlitherBot.GameState

data AvoidAiState = AvoidAiState
  { aasCurrentAngle :: !Double
  , aasLastCandidates :: ![(Double, Double)]
  , aasUtilityGrid :: !UtilityGrid
  }

type UgiRes = 256

ugiRes :: Int32
ugiRes = fromIntegral (natVal (Proxy :: Proxy UgiRes))

ugiSize :: Double
ugiSize = 2000

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
snakeBodyPartRadius = 30

foodRadius :: Double
foodRadius = 20

blurRadius :: Double
blurRadius = 500

utilityGrid :: SnakeId -> Position -> GameState -> UtilityGrid
utilityGrid ourSnakeId ourPosition GameState{..} =
  CV.exceptError $ do
    snakesAndFood :: UtilityGrid <- CV.createMat $ do
      mutMat <- emptyUtilityGrid
      forM_ (HMS.toList gsSnakes) $ \(snakeId, Snake{..}) -> do
        when (snakeId /= ourSnakeId) $
          forM_ (snakePosition : toList snakeBody) $ \pos ->
            forM_ (gridIndex ourPosition pos) $ \ix ->
              CV.circle mutMat
                ix
                (sizeToPixels snakeBodyPartRadius)
                (pure 1 :: V4 Double)
                (-1)
                CV.LineType_8
                0
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

possibleTurns :: [Double]
possibleTurns =
  [0] ++ turns ++ map negate turns
  where
    maxTurn = pi / 2
    turns = [ix * (maxTurn / 10) | ix <- [1..10]]

lookaheadDistance :: Double 
lookaheadDistance = 600

utilityGridLookup :: UtilityGrid -> V2 Int32 -> Double
utilityGridLookup ug ix = runST $ do
  mug <- CV.Unsafe.unsafeThaw ug
  CV.Unsafe.unsafeRead mug (map fromIntegral (toList ix))

pathUtility ::
     AvoidAiState
  -> Position
  -> Double
  -- ^ Angle in radians
  -> Double
pathUtility AvoidAiState{..} startPos angle =
  sum (map (utilityGridLookup aasUtilityGrid) indices)
  where
    indices = catMaybes (map (gridIndex startPos) poss)
    poss = 
      [ startPos + (Linear.angle angle ^* ((lookaheadDistance / fromIntegral steps) * fromIntegral step))
      | step <- [1..steps]
      ]
    steps = 10 :: Int

angleCandidates :: Position -> AvoidAiState -> [Double] -> [(Double, Double)]
angleCandidates pos aas@AvoidAiState{..} turns = utilities
  where
    angles =
      [ mod' (aasCurrentAngle + turn) (2 * pi)
      | turn <- turns
      ]

    utilities =
      [ (angle, pathUtility aas pos angle)
      | angle <- angles
      ]

bestAngle :: [(Double, Double)] -> Double
bestAngle = fst . minimumByEx (comparing snd)

avoidAi :: Ai AvoidAiState
avoidAi = Ai
  { aiInitialState = AvoidAiState 0 [] (CV.exceptError (CV.createMat emptyUtilityGrid))
  , aiUpdate = \gs@GameState{..} aas -> case gsOwnSnake of
      Nothing -> (AiOutput 0 False, aas{aasLastCandidates = []})
      Just ourSnakeId -> case HMS.lookup ourSnakeId gsSnakes of
        Nothing -> error ("Could not find our snake " ++ show ourSnakeId)
        Just snake -> let
          ourPosition = snakePosition snake
          ug = utilityGrid ourSnakeId ourPosition gs
          candidates = angleCandidates ourPosition aas possibleTurns
          angle = bestAngle candidates
          aas' = aas
            { aasUtilityGrid = ug
            , aasCurrentAngle = angle
            , aasLastCandidates = candidates
            }
          in (AiOutput angle False, aas')
  , aiHtmlStatus = \AvoidAiState{..} -> do
      Lucid.p_ (fromString (show aasCurrentAngle))
      let encodedImg =
            CV.exceptError $ do
              colorNormalized :: UtilityGrid <- CV.normalize 0 255 CV.Norm_MinMax Nothing aasUtilityGrid
              rgb <- CV.cvtColor CV.gray CV.rgb colorNormalized
              let pixelLength = lookaheadDistance * fromIntegral ugiRes / ugiSize
              rgbWithAngle <- CV.createMat $ do
                mutRgb <- CV.thaw rgb
                let startPoint :: V2 Int32 = pure (ugiRes `div` 2)
                forM_ aasLastCandidates $ \(angle, goodNess) -> do
                  let endPoint :: V2 Int32 =
                        round <$> ((fromIntegral <$> startPoint) + Linear.angle angle ^* pixelLength)
                  let blue = (1 - (goodNess + 1)/2) * 255
                  CV.line
                    mutRgb
                    startPoint
                    endPoint
                    (V4 blue 0 0 255 :: V4 Double)
                    2
                    CV.LineType_4
                    0
                return mutRgb
              CV.imencode (CV.OutputPng CV.defaultPngParams{CV.pngParamCompression = 0}) rgbWithAngle
      Lucid.img_
        [ Lucid.alt_ "Utility grid"
        , Lucid.src_ ("data:image/png;base64," <> T.decodeUtf8 (Base64.encode encodedImg))
        ]
  }
