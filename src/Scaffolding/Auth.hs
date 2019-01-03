{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -Wwarn -fno-warn-orphans #-}
module Scaffolding.Auth
       ( requiresRole
       , doAuth
       , doProfile
       , doProfileData
       ) where

import Control.Applicative  ((<$>))
import Control.Monad.Fail (MonadFail)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query')
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Happstack.Auth.Core.Auth (AuthState)
import Happstack.Auth.Core.AuthURL (AuthURL(A_Login))
import Happstack.Auth.Core.Profile (ProfileState)
import Happstack.Auth.Core.ProfileURL (ProfileURL(P_PickProfile))
import Happstack.Auth.Blaze.Templates   (handleAuth, handleProfile)
import Happstack.Server (Happstack, Response, escape, ToMessage)
-- ximport HJScript.Utils ()
import HSP
import Scaffolding.AppConf (HasAppConf(askAppConf))
import Scaffolding.MonadStack.Route (MonadRoute'(liftRoute, unRoute))
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import Scaffolding.Pages.Unauthorized (unauthorizedPage)
import qualified Scaffolding.ProfileData.Acid as ProfileData
import qualified Scaffolding.ProfileData.Parts as ProfileData
import qualified Scaffolding.MkURL as MkURL
import Scaffolding.ProfileData.User (MonadUser, MonadUserName, askAcidAuth, askAcidProfileData, lookMaybeUserId)
import Scaffolding.MkURL (MkURL)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Web.Routes (MonadRoute(askRouteFn), RouteT(unRouteT), nestURL, showURL)
import Web.Routes.Happstack (seeOtherURL)
import Web.Routes.RouteT (URL, liftRouteT)
import Web.Routes.XMLGenT (unUChild)

doAuth :: forall m v weburl.
          (Happstack (m AuthURL),
           MonadRoute (m AuthURL),
           MonadRoute' weburl v (m weburl),
           MonadRoute' AuthURL v (m AuthURL),
           HasAppConf (m weburl),
           MonadUserName (m weburl),
           MonadRoute (m weburl),
           MkURL weburl,
           MonadRender (m weburl),
           ToMessage (XMLType (m weburl)),
           Happstack (m weburl),
           EmbedAsAttr (m weburl) (Attr TL.Text weburl),
           URL (m weburl) ~ weburl,
           URL (m AuthURL) ~ AuthURL,
           Monad v
          ) =>  Maybe Text -> AuthURL -> m weburl Response
doAuth realm url =
    do conf <- askAppConf
       acidAuth <- askAcidAuth
       onAuthURL <- showURL (MkURL.profileURL P_PickProfile)
       showFn <- askRouteFn
       liftRoute' . nestURL MkURL.authURL . unRoute $ (handleAuth acidAuth (urlTemplate' showFn) realm onAuthURL url)
    where
      liftRoute' :: RouteT weburl v a -> m weburl a
      liftRoute' = liftRoute
      urlTemplate' :: (weburl -> [(Text, Maybe Text)] -> Text) -> String -> Html -> Html -> m AuthURL Response
      urlTemplate' = urlTemplate

-- | Instead of passing acidAuth etc we could just add AcidAuth etc to context.
doProfile :: forall m v weburl.
             (URL (m ProfileURL) ~ ProfileURL,
              URL (m weburl) ~ weburl,
              XMLType (m weburl) ~ XML,
              Happstack (m ProfileURL),
              MonadRoute (m ProfileURL),
              EmbedAsChild (m weburl) XML,
              EmbedAsAttr (m weburl) (Attr TL.Text weburl),
              MonadRender (m weburl),
              HasAppConf (m weburl),
              ToMessage (XMLType (m weburl)),
              MonadUser (m weburl), MkURL (URL (m weburl)),
              Happstack (m weburl),
              MonadRoute (m weburl),
              MonadRoute' weburl v (m weburl),
              MonadRoute' ProfileURL v (m ProfileURL),
              MonadFail (m ProfileURL),
              Monad v) =>
             AcidState AuthState
          -> AcidState ProfileState
          -> ProfileURL
          -> m weburl Response
doProfile acidAuth acidProfile profileURL =
    do postPickedURL <- showURL (MkURL.mkURL MkURL.CreateNew)
       showFn <- askRouteFn
       liftRoute' . nestURL MkURL.profileURL . unRoute' $ handleProfile acidAuth acidProfile (urlTemplate showFn) postPickedURL profileURL
    where
      liftRoute' :: RouteT weburl v a -> m weburl a
      liftRoute' = liftRoute
      unRoute' :: m ProfileURL a -> RouteT ProfileURL v a
      unRoute' = unRoute

doProfileData :: (MkURL.MkURL (URL m),
                  MonadRender m,
                  MonadUserName m,
                  HasAppConf m,
                  ToMessage (XMLType m),
                  EmbedAsAttr m (Attr TL.Text (URL m)),
                  MonadRoute m,
                  Happstack m) =>
                 AcidState AuthState
              -> AcidState ProfileState
              -> AcidState ProfileData.State
              -> MkURL.URL
              -> m Response
doProfileData _acidAuth _acidProfile _acidProfileData profileDataURL =
    do postCreateURL <- showURL (MkURL.mkURL MkURL.Edit)
       ProfileData.handle postCreateURL profileDataURL

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Html where
    asChild html = asChild (CDATA False (TL.pack $ renderHtml html))

requiresRole :: (URL m ~ weburl,
                 MkURL.MkURL (URL m),
                 MonadUserName m,
                 MonadRender m,
                 Happstack m,
                 MonadRoute m,
                 HasAppConf m,
                 EmbedAsAttr m (Attr TL.Text weburl),
                 ToMessage (XMLType m)
                ) => ProfileData.Role -> weburl -> m weburl
requiresRole role url =
    do mu <- lookMaybeUserId
       case mu of
         Nothing -> escape $ seeOtherURL (MkURL.authURL A_Login)
         (Just uid) ->
             do apd <- askAcidProfileData
                r <- query' apd (ProfileData.HasRole uid role)
                if r
                   then return url
                   else escape $ unauthorizedPage "You do not have permission to view this page."

urlTemplate :: forall headers body authurl weburl v m.
               (EmbedAsChild (RouteT authurl v) headers,
                EmbedAsChild (RouteT authurl v) body,
                MonadRoute' weburl v (m weburl),
                MonadRoute' authurl v (m authurl),
                MonadRender (m weburl),
                HasAppConf (m weburl),
                ToMessage (XMLType (m weburl)),
                -- XMLType (m weburl) ~ TL.Text,
                MonadUser (m weburl),
                Happstack (m weburl),
                MonadRoute (m weburl),
                EmbedAsAttr (m weburl) (Attr TL.Text (URL (m weburl))),
                Monad (m authurl),
                Monad v) =>
               (weburl -> [(Text, Maybe Text)] -> Text)
            -> String
            -> body
            -> headers
            -> m authurl Response
urlTemplate showFn title headers body =
    do headersXML <- liftRoute $ map unUChild <$> (unXMLGenT $ asChild headers :: RouteT authurl v [ChildType (RouteT authurl v)])
       bodyXML    <- liftRoute $ map unUChild <$> (unXMLGenT $ asChild body    :: RouteT authurl v [ChildType (RouteT authurl v)])
       liftRoute' $ unnest showFn $ unRoute $ template' title headersXML bodyXML
    where
      liftRoute' :: RouteT authurl v a -> m authurl a
      liftRoute' = liftRoute
      template' :: String -> [XML] -> [XML] -> m weburl Response
      template' = template

unnest :: (url1 -> [(Text, Maybe Text)] -> Text)
       -> RouteT url1 m a
       -> RouteT url2 m a
unnest showFn routeSP =
    liftRouteT $ unRouteT routeSP showFn
