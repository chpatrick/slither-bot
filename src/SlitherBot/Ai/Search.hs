{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SlitherBot.Ai.Search
  ( SearchAiState
  , searchAi
  ) where

import           Data.Monoid (Sum(..))
import           Linear hiding (angle, trace)
import qualified Linear
import qualified OpenCV as CV
import           Linear.V4 (V4)
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as T
import qualified Lucid.Html5 as Lucid
import           Data.Fixed (mod')
import qualified Control.Monad.Search as Search

import           SlitherBot.Prelude
import           SlitherBot.Ai
import           SlitherBot.Protocol
import           SlitherBot.GameState
import           SlitherBot.UtilityGrid

data SearchAiState = SearchAiState
  { sasCurrentAngle :: !Angle
  , sasChosenPath :: ![StepState]
  , sasChosenPathUtility :: !Utility
  , sasUtilityGrid :: !UtilityGrid
  }

type Utility = Double
type Angle = Double
type Distance = Double

firstStepDistance :: Distance
firstStepDistance = 50

stepDistanceIncreaseFactor :: Double
stepDistanceIncreaseFactor = 2

branchingFactor :: Int
branchingFactor = 5

treeDepth :: Int
treeDepth = 4

-- branchUtilityCutoff :: Utility
-- branchUtilityCutoff = 0.5

possibleTurns :: [Angle]
possibleTurns =
  [0] ++ turns ++ map negate turns
  where
    maxTurn = pi / 4
    turnsEachSide :: Int = branchingFactor `div` 2
    turns :: [Angle] =
      [fromIntegral ix * (maxTurn / fromIntegral turnsEachSide) | ix <- [1..turnsEachSide]]

data StepState = StepState
  { ssPosition :: !Position
  , ssDirection :: !Angle
  , ssStepsTaken :: !Int
  } deriving (Eq, Show)

possibleStepStates :: StepState -> [StepState]
possibleStepStates StepState{..} =
    [ StepState
        { ssPosition = ssPosition + Linear.angle angle ^* dist
        , ssDirection = angle
        , ssStepsTaken = ssStepsTaken + 1
        }
    | angle <- angles
    ]
  where
    dist = lastEx (take (ssStepsTaken + 1) (iterate (* stepDistanceIncreaseFactor) firstStepDistance))
    angles =
      [ mod' (ssDirection + turn) (2 * pi)
      | turn <- possibleTurns
      ]

speedupThreshold :: Utility
speedupThreshold = 4.999999 / 5

getPath :: V2 Int32 -> V2 Int32 -> [V2 Int32]
getPath startPos endPos =
  let
    numberOfSamples :: Int32
    numberOfSamples = floor (distance (fromIntegral <$> startPos) (fromIntegral <$> endPos) :: Double)
    step :: V2 Double
    step = (fromIntegral <$> (endPos - startPos)) ^/ fromIntegral numberOfSamples
  in
    [ startPos + (floor <$> fromIntegral x *^ step)
    | x <- [ 1 .. numberOfSamples :: Int32 ]
    ]

findBestAngle :: UtilityGrid -> Position -> StepState -> Maybe (Utility, [StepState])
findBestAngle ug0 ourPosition0 ss0 =
  case Search.runSearch (go [] ug0 ourPosition0 ss0) of
    [] -> Nothing
    (Sum utility, path) : _ -> Just (utility, reverse path)
  where
    go :: [StepState] -> UtilityGrid -> Position -> StepState -> Search.Search (Sum Utility) [StepState]
    go pathSoFar ug ourPosition ss = do
      if ssStepsTaken ss >= treeDepth
        then return (ss : pathSoFar)
        else asum
          [ do
              let
                costs = map (\ix -> 1 + max (-1) (utilityGridLookup ug ix)) (getPath startIndex endIndex)
                cost = sum costs / (fromIntegral (length costs))
              -- guard (cost < branchUtilityCutoff + 1)
              Search.cost' (Sum cost)
              go (ss' : pathSoFar) ug ourPosition ss'
          | ss' <- possibleStepStates ss
          , endIndex <- toList (gridIndex ourPosition (ssPosition ss'))
          , startIndex <- toList (gridIndex ourPosition (ssPosition ss))
          ]

{-
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
-}

searchAi :: Ai SearchAiState
searchAi = Ai
  { aiInitialState = SearchAiState 0 [] 0 (CV.exceptError (CV.createMat emptyUtilityGrid))
  , aiUpdate = \gs@GameState{..} sas -> case gsOwnSnake of
      Nothing -> (AiOutput 0 False, sas{sasChosenPath = []})
      Just ourSnakeId -> case HMS.lookup ourSnakeId gsSnakes of
        Nothing -> error ("Could not find our snake " ++ show ourSnakeId)
        Just snake -> let
          ourPosition = snakePosition snake
          ug = utilityGrid ourSnakeId ourPosition gs
          ss = StepState
            { ssPosition = ourPosition
            , ssDirection = sasCurrentAngle sas
            , ssStepsTaken = 0
            }
          (utility, bestPath) = case findBestAngle ug ourPosition ss of
            Nothing -> trace "NO PATHS FOUND" (0, [])
            Just x -> x
          bestAngle = case bestPath of
            _ : secondSS : _ -> ssDirection secondSS
            _ -> 0
          sas' = sas
            { sasUtilityGrid = ug
            , sasCurrentAngle = bestAngle
            , sasChosenPathUtility = utility
            , sasChosenPath = bestPath
            }
          speedup =
            not (null bestPath) &&
            utility / fromIntegral treeDepth < speedupThreshold
          in (AiOutput bestAngle speedup, sas')
  , aiHtmlStatus = \SearchAiState{..} -> do
      Lucid.p_ (fromString (show sasChosenPathUtility))
      Lucid.p_ (fromString (show (length sasChosenPath)))
      let encodedImg =
            CV.exceptError $ do
              normalized :: CV.Mat (CV.ShapeT '[UgiRes, UgiRes]) ('CV.S 1) ('CV.S Word8) <- CV.matConvertTo (Just 128) (Just 128) sasUtilityGrid
              rgb <- CV.applyColorMap CV.ColorMapJet normalized
              rgbWithAngle <- CV.createMat $ do
                mutRgb <- CV.thaw rgb
                case sasChosenPath of
                  [] -> return ()
                  startSS : pathTail -> do
                    let ourPos = ssPosition startSS
                    forM_ (zip sasChosenPath pathTail) $ \(ss1, ss2) -> do
                      case (gridIndex ourPos (ssPosition ss1), gridIndex ourPos (ssPosition ss2)) of
                        (Just startPoint, Just endPoint) ->
                          CV.line
                            mutRgb
                            startPoint
                            endPoint
                            (V4 255 0 0 255 :: V4 Double)
                            2
                            CV.LineType_4
                            0
                        _ -> return ()
                return mutRgb
              CV.imencode (CV.OutputPng CV.defaultPngParams{CV.pngParamCompression = 0}) rgbWithAngle
      Lucid.img_
        [ Lucid.alt_ "Utility grid"
        , Lucid.src_ ("data:image/png;base64," <> T.decodeUtf8 (Base64.encode encodedImg))
        ]
  }
