{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- | Monad to access the server's acid state.
module Scaffolding.MonadStack.Acid
    ( MonadAcid(liftAcid)
    ) where

import Control.Monad.Reader (ReaderT)

class Monad v => MonadAcid acid v m | m -> v where
    liftAcid :: ReaderT acid v a -> m a
