-- | Information which controls the overall appearance of the
-- | application.  (Suggest renaming Pages.AppTemplate -> Theme).
{-# LANGUAGE FlexibleContexts, FlexibleInstances, PackageImports, RankNTypes, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wwarn -F -pgmFhsx2hs -fno-warn-orphans -fno-warn-name-shadowing #-}
module Scaffolding.Pages.AppTemplate
    ( MonadRender
    , template'
    , template
    , lightTemplate
    , twoColumn
    ) where

import Control.Applicative ((<$>))
import "mtl" Control.Monad.Trans
import Control.Monad.Writer (MonadWriter)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.UserId (UserId(..))
import Happstack.Server (Happstack, ToMessage(..), Response, toResponse)
import Happstack.Server.HSP.HTML ()
import Happstack.Server.SURI ({- instance ToSURI Text -})
-- import HJScript.Utils ()
import HSP (XML, GenXML, XMLGenerator, unXMLGenT, EmbedAsChild, EmbedAsAttr, Attr(..), StringType, XMLType, asChild, asAttr, genElement, genEElement, fromStringLit)
import HSP.Google.Analytics(analytics)
-- import qualified HSX.XMLGenerator as HSX
-- import Language.HJavaScript.Syntax (Block)
import Prelude hiding (null)
import Scaffolding.AppConf (AppConf(uacct), HasAppConf(askAppConf, askTheme), Theme(..), menuBar)
-- import Scaffolding.HSP.Widget (WidgetGenerator, Widgets, Widget, Id, NextId(nextId), nextId')
import Scaffolding.MonadStack.Headers (MonadHeaders, listenHeaders)
import Scaffolding.ProfileData.User (MonadUser, lookMaybeUserId)
import Web.Routes.RouteT (URL, MonadRoute)
import Web.Routes.XMLGenT ()

class (Functor x,
       MonadHeaders IO x,
--       EmbedAsChild x (Block ()),
       EmbedAsChild x T.Text,
       EmbedAsChild x XML,
       EmbedAsChild x [XML],
       EmbedAsChild x (),
       EmbedAsAttr x (Attr String T.Text),
--       EmbedAsAttr x (Attr String Id),
       XMLGenerator x,
       StringType x ~ TL.Text
--       Widgets x
       ) => MonadRender x

template :: (HasAppConf m,
             ToMessage (XMLType m),
             MonadUser m,
             MonadRoute m,
             Happstack m,
             XMLGenerator m,
             EmbedAsAttr m (Attr TL.Text (URL m)),
             EmbedAsChild m headers,
             EmbedAsChild m XML,
             EmbedAsChild m [XML],
--             EmbedAsChild m (Block ()),
             EmbedAsChild m body,
             MonadWriter [XML] m,
             StringType m ~ TL.Text
             ) =>
            String -> headers -> body -> m Response
template title headers body =
    askTheme >>= \ theme ->
    toResponse <$> (unXMLGenT $ template' theme title headers body)

-- http://wiki.developers.facebook.com/index.php/Connect/Setting_Up_Your_Site
template' :: ( Happstack m
             , MonadUser m
             , MonadRoute m
             , MonadWriter [XML] m
             , XMLGenerator m
             , HasAppConf m
             , EmbedAsChild m body
             , EmbedAsChild m headers
--             , EmbedAsChild m (Block ())
             , EmbedAsChild m XML
             , EmbedAsChild m [XML]
             , EmbedAsAttr m (Attr TL.Text (URL m))
             , StringType m ~ TL.Text
             ) =>
             Theme m
          -> String
          -> headers
          -> body
          -> GenXML m
template' theme title headers body =
 do mUid <- lift $ lookMaybeUserId
    (_body', extraHeaders) <- listenHeaders (asChild body)
    lightTemplate' theme mUid title headers extraHeaders body


lightTemplate :: ( ToMessage (XMLType m)
                 , XMLGenerator m
                 , EmbedAsAttr m (Attr TL.Text (URL m))
                 , MonadRoute m
                 , HasAppConf m
                 , Functor m
                 , EmbedAsChild m headers
                 , EmbedAsChild m extraHeaders
                 , EmbedAsChild m body
--                 , EmbedAsChild m (Block ())
                 , EmbedAsChild m [XML]
                 , EmbedAsChild m XML
                 , StringType m ~ TL.Text
                 ) =>
                 Maybe UserId
              -> String
              -> headers
              -> extraHeaders
              -> body
              -> m Response
lightTemplate mUid title headers extraHeaders body =
    askTheme >>= \ theme ->
    toResponse <$> (unXMLGenT $ lightTemplate' theme mUid title headers extraHeaders body)

lightTemplate' :: ( MonadRoute m
                  -- , MonadRender m
                  , HasAppConf m
                  , XMLGenerator m
                  , EmbedAsAttr m (Attr TL.Text (URL m))
                  , EmbedAsChild m headers
                  , EmbedAsChild m extraheaders
                  , EmbedAsChild m body
--                  , EmbedAsChild m (Block ())
                  , EmbedAsChild m XML
                  , EmbedAsChild m [XML]
                  , StringType m ~ TL.Text
                  ) =>
                  Theme m
               -> Maybe UserId
               -> String
               -> headers
               -> extraheaders
               -> body
               -> GenXML m
lightTemplate' theme mUid title headers extraHeaders body =
    do conf <- lift askAppConf
       <html>
        <head>
          <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
          <link type="text/css" href="/theme/screen.css" rel="stylesheet" />
          <link type="text/css" href="/theme/menubar.css" rel="stylesheet" />
          <script type="text/javascript" src="/jquery/jquery.js" ></script>
          <script type="text/javascript" src="/jquery-ui/jquery-ui.js" ></script>
          <script type="text/javascript" src="/theme/json2.js" ></script>
          <% widgetHeaders theme %>
          <% headers %>
          -- <% extraHeaders %>
          <title><% TL.pack title %></title>
        </head>
        <body>
         <% menuBar =<< return (menu theme $ mUid) %>
         <% body %>
         <% footer theme %>
         <% maybe (return []) analytics (uacct conf) %>
        </body>
        </html>

twoColumn :: forall (m :: * -> *) c c1 c2 c3.
             (EmbedAsAttr m (Attr TL.Text TL.Text),
              EmbedAsChild m c2,
              EmbedAsChild m c1,
              EmbedAsChild m c3,
              EmbedAsChild m c,
              EmbedAsChild m (XMLType m),
              StringType m ~ TL.Text) =>
             c -> c3 -> c1 -> c2 -> [GenXML m]
twoColumn header footer left right =
    [<div id="two-column">
      <% header %>
      <div id="two-column-left">
       <% left %>
      </div>
      <div id="two-column-right">
       <% right %>
      </div>
      <% footer %>
     </div>]

-- instance MonadRender m => Widgets m
-- instance MonadRender m => WidgetGenerator m
-- instance MonadRender m => NextId (Widget m) where nextId = nextId'
