{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module SlitherBot.Prelude
  ( module All
  , MonadSlither
  ) where

import ClassyPrelude as All hiding (mapM_, toList)
import Prelude as All (mapM_, iterate)
import Data.Foldable as All (toList)
import Control.Monad.Logger as All
import Control.Concurrent.Async.Lifted.Safe as Async
import Control.Monad.Trans.Unlift (MonadBaseUnlift)

type MonadSlither m =
  (MonadIO m, MonadCatch m, MonadMask m, MonadLogger m, MonadBaseUnlift IO m, Async.Forall (Async.Pure m))
