module Scaffolding.Session
    ( requireSession
    ) where

requireSession :: m a -> m a
requireSession m = m
