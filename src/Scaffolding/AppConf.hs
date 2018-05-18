{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, PackageImports, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
module Scaffolding.AppConf
    ( AppConf(..)
    , LogMode(..)
    , defaultConf
    , parseConfig
    , Theme(..)
    , Menu(..)
    , MenuItem(..)
    , menuBar
    , HasAppConf(askAppConf, askTheme)
    ) where

import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Data.UserId (UserId(..))
import Happstack.Auth.Core.Profile
import Happstack.Server (Conf(validator, port), nullConf)
import HSP (XML, GenXML, GenChildList, XMLGenerator, EmbedAsAttr, Attr(..), StringType, asChild, asAttr, genElement, fromStringLit)
import HSP.XMLGenerator (XMLType)
import HSP.Google.Analytics (UACCT)
-- import Language.HJavaScript.Syntax (Block)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import Text.ParserCombinators.Parsec (parse, many1)
import Text.ParserCombinators.Parsec.Char (char, alphaNum, digit, spaces)
import Web.Routes.RouteT (URL)

data LogMode
    = Production
    | Development
      deriving (Read, Show, Eq, Ord, Enum, Bounded)

data AppConf
    = AppConf { httpConf   :: Conf
              , baseURI    :: Text
              , top        :: FilePath
              , static     :: FilePath
              , logs       :: FilePath
              , favicon    :: FilePath
              , logMode    :: LogMode
              , sshProxy   :: Bool
              , uacct      :: Maybe UACCT
              , addAdmin   :: [UserId]
              }

defaultConf :: FilePath -> String -> AppConf
defaultConf favicon progName
    = AppConf { httpConf = nullConf -- { port = maybe 80 (read . drop 1 . uriPort) $ uriAuthority (connectURL facebookConfig)  }
              , baseURI  = pack (progName ++ ": missing --base-uri option")
              , top      = "_local"
              , static   = "."
              , logs     = "_local"
              , logMode  = Development
              , favicon  = favicon
              , sshProxy = False
              , uacct    = Nothing
              , addAdmin = []
              }

type Flag = AppConf -> AppConf

opts :: String -> Maybe UACCT -> [OptDescr Flag]
opts appName appUACCT =
       [ Option [] ["http-port"]        (ReqArg (\h -> \c -> c { httpConf = (httpConf c) {port = read h} }) "port") "port to bind http server"
       , Option [] ["base-uri"]         (ReqArg (\h -> \c -> c {baseURI = pack h}) "uri") "http://servername:<port>/"
       , Option [] ["no-validate"]      (NoArg (       \c -> c { httpConf = (httpConf c) { validator = Nothing } })) "Turn off HTML validation"
       , Option [] ["top"]              (ReqArg (\h -> \c -> c {top = h}) "PATH") "The top of the directory tree where the app can write files."
       , Option [] ["static"]           (ReqArg (\h -> \c -> c {static = h}) "PATH") "The directory searched for static files"
       , Option [] ["logs"]             (ReqArg (\h -> \c -> c {logs = h}) "PATH") "The directory to store log files in"
       , Option [] ["log-mode"]         (ReqArg (\h -> \c -> c {logMode = read h}) (show ([minBound .. maxBound] :: [LogMode]))) "The logging mode to use"
       , Option [] ["enable-analytics"] (NoArg  (      \c -> c { uacct = appUACCT })) "Enable google analytics tracking."
       , Option [] ["add-admin"]        (ReqArg (\h -> \c -> c {addAdmin = UserId (read h) : addAdmin c}) "userId") "Make a user an Admin and then exit."
       , Option [] ["favicon"]          (ReqArg (\h -> \c -> c {favicon = h}) "PATH") "Specify where to find the favicon file relative to the static directory"
       ]

parseConfig :: [String] -> String -> Maybe UACCT -> Either [String] (AppConf -> AppConf)
parseConfig args appName appUACCT
    = case getOpt Permute (opts appName appUACCT) args of
        (flags,_,[]) ->
            let modAppConf = \appConf -> foldr ($) appConf flags
            in Right modAppConf
        (_,_,errs)   -> Left (errs ++ [usageInfo "usage:" (opts appName Nothing)] )

data MenuItem url = MenuItem String url
data Menu url = Menu (MenuItem url)

menuBar ::
    forall m a. ( XMLGenerator m , EmbedAsAttr m (Attr TL.Text a) , StringType m ~ TL.Text , XMLType m ~ XML)
    => [Menu a]
    -> GenXML m
menuBar [] = <div id="menubar"></div>
menuBar menus =
    <div id="menubar">
      <ul>
       <% mapM mkMenu menus %>
      </ul>
    </div>
    where
      mkMenu :: Menu a -> GenXML m
      mkMenu (Menu (MenuItem name loc)) = <li><a href=loc><% TL.pack name %></a></li>

data Theme m
    = Theme
      { menu :: Maybe UserId -> [Menu (URL m)]
      , footer :: GenXML m -- XMLGenT m (XMLType m)
      , widgetHeaders :: GenChildList m
      }

class HasAppConf m where
    askAppConf :: m AppConf
    askTheme :: m (Theme m)
