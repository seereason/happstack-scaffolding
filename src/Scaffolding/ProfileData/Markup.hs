{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -F -pgmFtrhsx #-}
{-# OPTIONS -Wall -Werror #-}
{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
module Scaffolding.ProfileData.Markup where

import Control.Monad.Trans (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Happstack.Auth.Core.Profile (UserId(..))
import HSP (XMLGenerator, EmbedAsAttr(..), genElement, asChild, Attr((:=)), asAttr)
import qualified HSX.XMLGenerator as HSX
import Scaffolding.ProfileData.URL (MkURL(userURL))
import Scaffolding.ProfileData.User (MonadUserName, userName)
import Web.Routes (showURL)
import Web.Routes.RouteT (MonadRoute, URL)

userMarkup :: (MonadRoute m, MonadUserName m, MkURL (URL m), MonadIO m, XMLGenerator m, EmbedAsAttr m (Attr String Text)) =>
              UserId -> HSX.XMLGenT m (HSX.XML m)
userMarkup u =
    do name <- userName u
       link <- userLink u
       <span><% link %>:<% either show T.unpack name %></span>

userLink :: (MonadRoute m, MonadUserName m, MkURL (URL m), XMLGenerator m, EmbedAsAttr m (Attr String Text)) =>
            UserId -> HSX.XMLGenT m (HSX.XML m)
userLink u =
    do url <- showURL (userURL u)
       <a href=url>U<% show (unUserId u) %></a>
