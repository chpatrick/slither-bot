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

import           ClassyPrelude hiding (toList)
import           Data.Foldable (toList)
import           Prelude (iterate)
import           Data.Monoid (Sum(..))
import           Control.Lens ((^.))
import           Linear hiding (angle, trace)
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
import qualified Control.Monad.Search as Search

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

stepDistance :: Distance
stepDistance = 100

branchingFactor :: Int
branchingFactor = 5

treeDepth :: Int
treeDepth = 5

branchUtilityCutoff :: Utility
branchUtilityCutoff = 0.5

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
        { ssPosition = ssPosition + Linear.angle angle ^* stepDistance
        , ssDirection = angle
        , ssStepsTaken = ssStepsTaken + 1
        }
    | angle <- angles
    ]
  where
    angles =
      [ mod' (ssDirection + turn) (2 * pi)
      | turn <- possibleTurns
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
              let cost = 1 + max (-1) (utilityGridLookup ug ix)
              guard (cost < branchUtilityCutoff + 1)
              Search.cost' (Sum cost)
              go (ss' : pathSoFar) ug ourPosition ss'
          | ss' <- possibleStepStates ss
          , ix <- toList (gridIndex ourPosition (ssPosition ss'))
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
          in (AiOutput bestAngle False, sas')
  , aiHtmlStatus = \SearchAiState{..} -> do
      Lucid.p_ (fromString (show sasChosenPathUtility))
      Lucid.p_ (fromString (show (length sasChosenPath)))
      let encodedImg =
            CV.exceptError $ do
              colorNormalized :: UtilityGrid <- CV.normalize 0 255 CV.Norm_MinMax Nothing sasUtilityGrid
              rgb <- CV.cvtColor CV.gray CV.rgb colorNormalized
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
