#!/usr/bin/env runghc

module Main where

import Data.Char (isDigit)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import Distribution.Verbosity
import System.Cmd
import System.Exit
import System.IO
import System.Posix.Files (fileExist)

trhsxProgram = simpleProgram "trhsx"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
         hookedPrograms = [trhsxProgram]
       , buildHook = buildHook simpleUserHooks
                     -- Increase cabal verbosity during build to see ghc command line
                     -- { buildHook = \ desc info hooks flags -> (buildHook simpleUserHooks) desc info hooks (flags {buildVerbosity = Flag verbose}) } 
       }

runProof lbi =
    system (buildDir lbi ++ "/proof/proof") >>= putStrLn . show
