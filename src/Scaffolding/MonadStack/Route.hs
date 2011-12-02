{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Scaffolding.MonadStack.Route
    ( MonadRoute'(liftRoute, unRoute)
    ) where

import Web.Routes.RouteT (RouteT)

class MonadRoute' url v m | m -> v where
    liftRoute :: Monad v => RouteT url v a -> m a
    unRoute :: Monad v => m a -> RouteT url v a
