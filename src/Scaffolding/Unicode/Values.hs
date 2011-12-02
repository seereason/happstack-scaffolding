{-# LANGUAGE TemplateHaskell #-}
module Scaffolding.Unicode.Values where

import Scaffolding.Unicode.Render

-- This generates a large number of values of type Char.  The value
-- names are camelCase form generated from the official Unicode
-- character description.  See module UnicodeRender for the rest of
-- the details.

$(return $ concat $ map (uncurry makeSym) charsOfInterest)


