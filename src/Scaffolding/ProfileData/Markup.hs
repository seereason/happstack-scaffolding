{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -F -pgmFhsx2hs #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
module Scaffolding.ProfileData.Markup where

import Control.Monad.Trans (MonadIO)
import qualified Data.Text.Lazy as TL
import Data.UserId (UserId(..))
import HSP (GenXML, XMLGenerator, EmbedAsAttr(..), StringType, genElement, asChild, Attr((:=)), asAttr, fromStringLit)
import Scaffolding.MkURL (MkURL(userURL))
import Scaffolding.ProfileData.User (MonadUserName, userName)
import Web.Routes (showURL)
import Web.Routes.RouteT (MonadRoute, URL)

userMarkup :: (MonadRoute m, MonadUserName m, MkURL (URL m), MonadIO m, XMLGenerator m, StringType m ~ TL.Text) =>
              UserId -> GenXML m
userMarkup u =
    do name <- userName u
       link <- userLink u
       <span><% link %>:<% either (TL.pack . show) TL.fromStrict name %></span>

userLink :: (MonadRoute m, MkURL (URL m), XMLGenerator m, StringType m ~ TL.Text) =>
            UserId -> GenXML m
userLink u =
    do url <- showURL (userURL u)
       <a href=(TL.fromStrict url)>U<% TL.pack $ show (_unUserId u) %></a>
