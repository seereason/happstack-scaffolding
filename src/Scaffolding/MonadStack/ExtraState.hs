{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- | Monad to save extra headers we want to insert in our XML output.
module Scaffolding.MonadStack.ExtraState
    ( MonadExtraState(liftExtraState)
    ) where

import Control.Monad.State (StateT)

class MonadExtraState extra v m | m -> v where
    liftExtraState :: Monad v => StateT extra v a -> m a
