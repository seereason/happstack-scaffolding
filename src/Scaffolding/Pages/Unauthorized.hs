{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Scaffolding.Pages.Unauthorized
    ( unauthorizedPage
    ) where

import qualified Data.Text.Lazy as TL
import Happstack.Server (Happstack, Response, unauthorized, ToMessage)
import HSP (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XMLGenT, XMLType, genElement, fromStringLit)
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import Scaffolding.ProfileData.User (MonadUser)
import Web.Routes.RouteT (MonadRoute, URL)


unauthorizedPage :: (Happstack m,
                     MonadRoute m,
                     MonadUser m,
                     MonadRender m,
                     HasAppConf m,
                     HasAppConf (XMLGenT m),
                     EmbedAsAttr m (Attr TL.Text (URL m)),
                     ToMessage (XMLType m)) =>
                    String -> m Response
unauthorizedPage msg =
    do unauthorized =<< template "Unauthorized" ()
          <div id="main">
           <h1>Unauthorized</h1>
           <p><% TL.pack msg %></p>
          </div>

