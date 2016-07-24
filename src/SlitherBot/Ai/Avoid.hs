{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SlitherBot.Ai.Avoid
  ( AvoidAiState
  , avoidAi
  ) where

import           ClassyPrelude hiding (toList)
import           Data.Foldable (toList)
import           Prelude (iterate)
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
import           SlitherBot.UtilityGrid

data AvoidAiState = AvoidAiState
  { aasCurrentAngle :: !Double
  , aasLastCandidates :: ![(Double, Double)]
  , aasUtilityGrid :: !UtilityGrid
  }

possibleTurns :: [Double]
possibleTurns =
  [0] ++ turns ++ map negate turns
  where
    maxTurn = pi / 2
    turns = [ix * (maxTurn / 10) | ix <- [1..10]]

lookaheadDistance :: Double 
lookaheadDistance = 400

pathUtility ::
     AvoidAiState
  -> Position
  -> Double
  -- ^ Angle in radians
  -> Double
pathUtility AvoidAiState{..} startPos angle =
  sum (zipWith (*) (map (utilityGridLookup aasUtilityGrid) indices) (iterate (/ 1.2) 1))
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

normalizeMinMax :: [Double] -> [Double]
normalizeMinMax = \case
  [] -> []
  xs -> let
    minXs = minimumEx xs
    maxXs = maximumEx xs
    span = maxXs - minXs
    in
      [ (x - minXs) / span
      | x <- xs
      ]

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
                let utilities = map snd aasLastCandidates
                let utilitiesColors = map ((*255) . (\x -> 1 - x)) (normalizeMinMax utilities)
                forM_ (zip (map fst aasLastCandidates) utilitiesColors) $ \(angle, color) -> do
                  let endPoint :: V2 Int32 =
                        round <$> ((fromIntegral <$> startPoint) + Linear.angle angle ^* pixelLength)
                  CV.line
                    mutRgb
                    startPoint
                    endPoint
                    (V4 0 color (255 - color) 255 :: V4 Double)
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
