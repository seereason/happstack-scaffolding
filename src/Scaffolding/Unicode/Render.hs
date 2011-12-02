{-# OPTIONS -Wwarn #-}
module Scaffolding.Unicode.Render
    ( charsOfInterest
    , textToCamel
    , makeSym
    , searchByRegex
    , searchByWord
    , searchByName
    ) where

import Data.Char
import Data.Maybe
-- import Data.List
-- import Data.Char.Properties
import Data.Char.Properties.Names
import Language.Haskell.TH.Syntax
import Text.Regex

-- |makeSym creates a value of type Char using Template Haskell.
makeSym :: String -> Char -> [Dec]
makeSym s c = let x = mkName s; ch = mkName "Char" in [SigD x (ConT ch), ValD (VarP x) (NormalB (LitE (CharL c))) []]


allchars :: [Char]
allchars = map chr [0..0x10FFFF]

-- |searchByRegex -- filter unicode characters by regular expression, matching
-- on their description as defined in Data.Char.Properties.Names
searchByRegex :: Regex -> [Char]
searchByRegex rx = filter  f allchars
    where f :: Char -> Bool
          f c = isJust $ matchRegex rx (getCharacterName c)

searchByName :: String -> [Char]
searchByName s = filter f allchars
    where f :: Char -> Bool
          f c = s == getCharacterName c


-- undefined -- map getCharacterName (regexToChars (mkRegexWithOpts s True False))

-- |searchByWord -- filter unicode characters by whether a particular
-- word appears in their description, as defined in
-- Data.Char.Properties.Names
searchByWord :: String -> [Char]
searchByWord w = filter f allchars
    where f :: Char -> Bool
          f c = w `elem` (words (getCharacterName c))

-- |textToCamel -- convert a Unicode Character description into a
-- CamelCase haskell symbol name.  Non-alphanumeric characters are
-- replaced by underscores.
textToCamel :: String -> String
textToCamel =
    clean . concat . f . words
        where f [] = []
              f (x:xs) = (map toLower x) : (map capitalize xs)
              capitalize [] = []
              capitalize (c:cs) = (toUpper c) : (map toLower cs)
              clean = map (\c -> if isAlphaNum c then c else '_')

-- Look, a function with a unicode name!
-- (∩) :: (Eq a) => [a] -> [a] -> [a]
-- xs ∩ ys = nub $ xs ++ ys
{-
u :: (Eq a) => [a] -> [a] -> [a]
u xs ys = nub $ xs ++ ys
-}

-- |charsOfInterest -- Unicode characters to be used in
-- applications. This list will eventually be pared down to exactly
-- what we use and what we don't, so that we can generate a key
-- automatically.
charsOfInterest :: [(String,Char)]
charsOfInterest = charsOfInterestOpt
{-
charsOfInterest = charsofInterestComp

charsOfInterestComp :: [(String,Char)]
charsOfInterestComp = 
    map (\c -> (textToCamel $ getCharacterName c, c)) $ charsOfInterest'
-}

charsOfInterestOpt :: [(String,Char)]
charsOfInterestOpt = 
    [("forAll",'\8704')
    ,("complement",'\8705')
    ,("therefore",'\8756')
    ,("because",'\8757')
    ,("logicalAnd",'\8743')
    ,("logicalOr",'\8744')
    ,("equivalentTo",'\8781')
    ,("fullwidthLeftCurlyBracket",'\65371')
    ,("fullwidthRightCurlyBracket",'\65373')
    ,("endOfProof",'\8718')
    ,("rightTack",'\8866')
    ,("leftTack",'\8867')
    ,("assertion",'\8870')
    ,("doesNotProve",'\8876')
    ,("n_aryLogicalAnd",'\8896')
    ,("n_aryLogicalOr",'\8897')
    ,("n_aryIntersection",'\8898')
    ,("n_aryUnion",'\8899')
    ,("xor",'\8891')
    ,("nor",'\8893')
    ,("nand",'\8892')
    ,("rightwardsDoubleArrow",'\8658')
    ,("leftwardsDoubleArrow",'\8656')
    ,("leftRightDoubleArrow",'\8660')
    ,("equalsSign",'=')
    ,("less_thanSign",'<')
    ,("thereExists",'\8707')
    ,("thereDoesNotExist",'\8708')
    ,("skullAndCrossbones",'\9760')
    ,("starAndCrescent",'\9770')
    ,("hammerAndSickle",'\9773')
    ,("true",'\8872')
    ,("notTrue",'\8877')
    ,("elementOf",'\8712')
    ,("identicalTo",'\8801')
    ,("subsetOf",'\8834')
    ,("subsetOfOrEqualTo",'\8838')
    ,("supersetOf",'\8835')
    ,("supersetOfOrEqualTo",'\8839')
    ,("emptySet",'\8709')
    ,("notEqualTo",'\8800')
    ,("tilde",'~')
    ,("circledPlus",'\8853')
    ,("notSign",'\172')
    ,("ballotBox",'\9744')
    ,("ballotBoxWithCheck",'\9745')
    ,("ballotBoxWithX",'\9746')
    ,("geometricallyEquivalentTo",'\8782')
    ,("strictlyEquivalentTo",'\8803')
    ,("notEquivalentTo",'\8813')
    ,("less_thanOrEquivalentTo",'\8818')
    ,("greater_thanOrEquivalentTo",'\8819')
    ,("neitherLess_thanNorEquivalentTo",'\8820')
    ,("neitherGreater_thanNorEquivalentTo",'\8821')
    ,("precedesOrEquivalentTo",'\8830')
    ,("succeedsOrEquivalentTo",'\8831')
    ,("less_thanButNotEquivalentTo",'\8934')
    ,("greater_thanButNotEquivalentTo",'\8935')
    ,("precedesButNotEquivalentTo",'\8936')
    ,("succeedsButNotEquivalentTo",'\8937')
    ,("equivalentWithFourDotsAbove",'\10872')
    ,("congruentWithDotAbove",'\10861')
    ,("leftSingleQuotationMark",'\8216')
    ,("rightSingleQuotationMark",'\8217')
    ,("leftDoubleQuotationMark",'\8220')
    ,("rightDoubleQuotationMark",'\8221')
    ,("whiteHeartSuit",'\9825')
    ,("blackHeartSuit",'\9829')
    ]


{-
charsOfInterest' :: [Char]
charsOfInterest' = foldr u []
                   [ searchByName "FOR ALL"
                   , searchByName "COMPLEMENT"
                   , searchByName "THEREFORE"
                   , searchByName "BECAUSE"
                   , searchByName "LOGICAL AND"
                   , searchByName "LOGICAL OR"
                   , searchByName "EQUIVALENT TO"
                   , searchByName "FULLWIDTH LEFT CURLY BRACKET"
                   , searchByName "FULLWIDTH RIGHT CURLY BRACKET"
                   , searchByName "END OF PROOF"
                   , searchByName "RIGHT TACK" -- 
                   , searchByName "LEFT TACK"
                   , searchByName "ASSERTION"
                   , searchByName "DOES NOT PROVE"
                   , searchByName "N-ARY LOGICAL AND"
                   , searchByName "N-ARY LOGICAL OR"
                   , searchByName "N-ARY INTERSECTION"
                   , searchByName "N-ARY UNION"
                   , searchByName "XOR"
                   , searchByName "NOR"
                   , searchByName "NAND"
                   , searchByName "RIGHTWARDS DOUBLE ARROW"
                   , searchByName "LEFTWARDS DOUBLE ARROW"
                   , searchByName "LEFT RIGHT DOUBLE ARROW"
                   , searchByName "EQUALS SIGN"
                   , searchByName "LESS-THAN SIGN"
                   , searchByName "THERE EXISTS"
                   , searchByName "THERE DOES NOT EXIST"
                   , searchByName "SKULL AND CROSSBONES"
                   , searchByName "STAR AND CRESCENT"
                   , searchByName "HAMMER AND SICKLE"
                   , searchByName "TRUE"
                   , searchByName "NOT TRUE"
                   , searchByName "ELEMENT OF"
                   , searchByName "IDENTICAL TO"
                   , searchByName "SUBSET OF"
                   , searchByName "SUBSET OF OR EQUAL TO"
                   , searchByName "SUPERSET OF"
                   , searchByName "SUPERSET OF OR EQUAL TO"
                   , searchByName "EMPTY SET"
                   , searchByName "NOT EQUAL TO"
                   , searchByName "TILDE"
                   , searchByName "CIRCLED PLUS"
                   , searchByName "NOT SIGN"
                   , searchByRegex (mkRegex "BALLOT BOX")
                   , searchByRegex (mkRegex "EQUIV")
                   , searchByRegex (mkRegex "CONGRUENT")
                   , searchByName "LEFT SINGLE QUOTATION MARK"
                   , searchByName "RIGHT SINGLE QUOTATION MARK"
                   , searchByName "LEFT DOUBLE QUOTATION MARK"
                   , searchByName "RIGHT DOUBLE QUOTATION MARK"
{-
                   , searchByWord "AND"
                   , searchByWord "OR"
                   , searchByWord "NOT"
                   , searchByWord "INTERSECTION"
                   , searchByWord "UNION"
                   , searchByWord "SUBSET"
                   , searchByWord "SUPERSET"
                   , searchByWord "LOGICAL"
                   , searchByWord "CHECK"
                   , searchByWord "TRUE"
                   , searchByWord "ELEMENT"
-}
                   ]


pp :: [Char] -> IO ()
pp = (mapM_ print) . (map getCharacterName)

cheatSheet :: [(String, String)]
cheatSheet = map (\c -> let n = getCharacterName c in (n, textToCamel n)) charsOfInterest'

printCheatSheet :: FilePath -> IO ()
printCheatSheet fp = writeFile fp (intercalate "\n" $ map show cheatSheet)

-- |searchByUnicodeValue -- lookup from hex string.
searchByUnicodeValue :: String -> [Char]
searchByUnicodeValue s = getCharacterName $ chr (read s)

-}