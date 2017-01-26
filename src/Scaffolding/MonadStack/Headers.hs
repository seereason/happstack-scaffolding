{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, StandaloneDeriving, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- | Monad to save extra headers we want to insert in our XML output.
module Scaffolding.MonadStack.Headers
    ( MonadHeaders(liftHeaders)
    , HeadersT
    , tellHeaders
    , listenHeaders
    , runHeadersT
    ) where

import Control.Monad.Writer (MonadWriter(tell, listen), WriterT(runWriterT))
import HSP (XMLGenT(..), XML)

type A = [XML]

type HeadersT = WriterT A

class MonadWriter A m => MonadHeaders v m | m -> v where
    liftHeaders :: HeadersT v a -> m a

tellHeaders :: (MonadHeaders v m, Monad v) => A -> m ()
tellHeaders = liftHeaders . tell

listenHeaders :: MonadWriter A m => m a -> m (a, A)
listenHeaders = listen

runHeadersT :: (Functor m) => WriterT w m a -> m a
runHeadersT = fmap fst . runWriterT

deriving instance MonadHeaders v m => MonadHeaders v (XMLGenT m)
