{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Scaffolding.Pages.InternalServerError
    ( internalServerErrorPage
    ) where

import qualified Data.Text.Lazy as TL
import Happstack.Server (Happstack, Response, unauthorized, ToMessage)
import HSP (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT, XMLType, genElement, fromStringLit)
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import Scaffolding.ProfileData.User (MonadUser)
import Web.Routes.RouteT (MonadRoute, URL)

internalServerErrorPage :: (Happstack m, MonadRoute m, MonadUser m, MonadRender m, HasAppConf m, EmbedAsAttr m (Attr TL.Text (URL m)), ToMessage (XMLType m)) => String -> m Response
internalServerErrorPage msg =
    do unauthorized =<< template "Internal Server Error" ()
          <div id="main">
           <h1>Internal Server Error</h1>
           <p><% TL.pack msg %></p>
          </div>

