{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, PackageImports, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
{-# OPTIONS_GHC -Wall #-}
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

import qualified Data.Text as Text
import Data.Text (Text)
import Happstack.Auth.Core.Profile
import Happstack.Server (Conf(validator, port), nullConf)
import HSP (XMLGenT(..))
import HSP.Google.Analytics (UACCT)
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), ArgOrder(Permute), OptDescr(Option), getOpt)
import Text.ParserCombinators.Parsec (parse, many1)
import Text.ParserCombinators.Parsec.Char (char, alphaNum, digit, spaces)
import Web.Authenticate.Facebook (Facebook(..))

import HSP (XMLGenerator, EmbedAsChild, EmbedAsAttr, Attr(..), asChild, asAttr, genElement)
import qualified HSX.XMLGenerator as HSX
import Language.HJavaScript.Syntax (Block)
import Web.Routes.RouteT (URL, MonadRoute)

data LogMode
    = Production
    | Development
      deriving (Read, Show, Eq, Ord, Enum, Bounded)

data AppConf
    = AppConf { httpConf   :: Conf
              , baseURI    :: Text
              , store      :: FilePath
              , static     :: FilePath 
              , imageStore :: FilePath
              , imageCache :: FilePath
              , logs       :: FilePath
              , logMode    :: LogMode
              , facebook   :: Maybe Facebook
              , sshProxy   :: Bool
              , uacct      :: Maybe UACCT
              , addAdmin   :: [UserId]
              }

defaultConf :: Maybe Facebook -> String -> AppConf
defaultConf appFacebook progName
    = AppConf { httpConf = nullConf -- { port = maybe 80 (read . drop 1 . uriPort) $ uriAuthority (connectURL facebookConfig)  }
              , baseURI  = (Text.pack "/")
              , store    = "_local/" ++ progName ++ "_state"
              , static   = "."
              , imageStore = "_local/imageStore"
              , imageCache = "_local/imageCache"
              , logs     = "_local"
              , logMode  = Development
              , facebook = appFacebook
              , sshProxy = False
              , uacct    = Nothing
              , addAdmin = []
              }

type Flag = AppConf -> AppConf

opts :: Maybe UACCT -> [OptDescr Flag]
opts appUACCT =
       [ Option [] ["http-port"]        (ReqArg (\h -> \c -> c { httpConf = (httpConf c) {port = read h} }) "port") "port to bind http server"
       , Option [] ["base-uri"]         (ReqArg (\h -> \c -> c {baseURI = Text.pack h}) "uri") "http://servername:<port>/"
       , Option [] ["no-validate"]      (NoArg (       \c -> c { httpConf = (httpConf c) { validator = Nothing } })) "Turn off HTML validation"
       , Option [] ["store"]            (ReqArg (\h -> \c -> c {store = h}) "PATH") "The directory used for database storage."
       , Option [] ["static"]           (ReqArg (\h -> \c -> c {static = h}) "PATH") "The directory searched for static files" 
       , Option [] ["logs"]             (ReqArg (\h -> \c -> c {logs = h}) "PATH") "The directory to store log files in"
       , Option [] ["log-mode"]         (ReqArg (\h -> \c -> c {logMode = read h}) (show ([minBound .. maxBound] :: [LogMode]))) "The logging mode to use"
       , Option [] ["enable-analytics"] (NoArg  (      \c -> c { uacct = appUACCT })) "Enable google analytics tracking."
       , Option [] ["facebook-config"]  (ReqArg setFacebookConfig "app_id, app_secret") "Facebook app credentials"
       , Option [] ["add-admin"]        (ReqArg (\h -> \c -> c {addAdmin = UserId (read h) : addAdmin c}) "userId") "Make a user an Admin and then exit."
       ]
           where
             setFacebookConfig h =
                 let p = do appId <- many1 digit
                            _ <- char ','
                            spaces
                            appSecret <- many1 alphaNum
                            return (Facebook (Text.pack appId) (Text.pack appSecret) Text.empty)
                 in case parse p h h of
                      (Left e) -> error $ show e
                      (Right f) -> \c -> c { facebook = Just f }

parseConfig :: [String] -> Maybe UACCT -> Either [String] (AppConf -> AppConf)
parseConfig args appUACCT
    = case getOpt Permute (opts appUACCT) args of
        (flags,_,[]) -> 
            let modAppConf = \appConf -> foldr ($) appConf flags
            in Right modAppConf
        (_,_,errs)   -> Left errs

data MenuItem url = MenuItem String url
data Menu url = Menu (MenuItem url)

menuBar :: ( XMLGenerator m
           , EmbedAsAttr m (Attr String a)) =>
           [Menu a] -> XMLGenT m (HSX.XML m)
menuBar [] = <div id="menubar"></div>
menuBar menus =
    <div id="menubar">
      <ul>
       <% mapM mkMenu menus %>
      </ul>
    </div>
    where
      mkMenu (Menu (MenuItem name loc)) = <li><a href=loc><% name %></a></li>

data (EmbedAsChild m (Block ()), MonadRoute m, XMLGenerator m) => Theme m
    = Theme
      { menu :: Maybe UserId -> [Menu (URL m)]
      , footer :: XMLGenT m (HSX.XML m)
      , widgetHeaders :: XMLGenT m [HSX.Child m]
      }

class HasAppConf m where
    askAppConf :: m AppConf
    askTheme :: m (Theme m)

deriving instance HasAppConf m => HasAppConf (XMLGenT m)
