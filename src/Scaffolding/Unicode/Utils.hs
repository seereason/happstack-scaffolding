module Scaffolding.Unicode.Utils
    ( doubleQuote
    ) where

import qualified Scaffolding.Unicode.Values as UV

doubleQuote :: String -> String
doubleQuote s = [UV.leftDoubleQuotationMark] ++ s ++ [UV.rightDoubleQuotationMark]
