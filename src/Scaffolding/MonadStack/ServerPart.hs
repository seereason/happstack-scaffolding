{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- | Monad to save extra headers we want to insert in our XML output.
module Scaffolding.MonadStack.ServerPart
    ( MonadServerPart(liftServerPart)
    ) where

import Happstack.Server (ServerPartT)

class MonadServerPart v m | m -> v where
    liftServerPart :: Monad v => ServerPartT v a -> m a
