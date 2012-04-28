{-# LANGUAGE FlexibleContexts, PackageImports, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
module Scaffolding.Pages.Common 
    ( seeOtherXML
    , seeOtherResponse
    , seeOtherURLParams
    , fbml
    , makeForm
    ) where

import Data.Generics.SYB.WithClass.Instances ()
import Data.Text (Text)
import Happstack.Server (Happstack, Input, seeOther, ToMessage(..), Response, toResponse)
--import Happstack.Server.Formlets (handleFailure)
import HJScript.Utils ()
import HSP hiding (escape)
import qualified HSX.XMLGenerator as HSX
import Prelude hiding (null)
import Scaffolding.Pages.AppTemplate (MonadRender)
import Scaffolding.Pages.FormPart (multiFormPart)
import Text.Digestive (Form)
import Web.Routes (showURL, showURLParams)
import Web.Routes.RouteT (MonadRoute, URL)
import Web.Routes.XMLGenT ()

seeOtherXML :: (Happstack m, MonadRender m) => String -> GenXML m
seeOtherXML loc = (seeOther loc =<< (<a href=loc><%  loc %></a>))

seeOtherResponse :: (Happstack m, MonadRender m, ToMessage (HSX.XMLType m)) => String -> m Response
seeOtherResponse = fmap toResponse . unXMLGenT . seeOtherXML

-- | move to Web.Routes.Happstack
seeOtherURLParams :: (Happstack m, MonadRoute m, MonadRender m) => URL m -> [(Text, Maybe Text)] -> res -> m res
seeOtherURLParams url params res = 
    do otherURL <- showURLParams url params
       seeOther otherURL res

fbml :: (MonadRender m, ToMessage a) => XMLGenT m a -> m Response
fbml = fmap toResponse . unXMLGenT

-- |A helper function for creating forms, makes the types a little less wriggly.
makeForm :: (Happstack m, MonadRoute m, MonadRender m, ToMessage (HSX.XMLType m), EmbedAsChild m xml) =>
            String
         -> URL m
         -> m (Form m [Input] e xml a)
         -> (a -> m Response)
         -> m [GenXML m]
makeForm formName here makeFormlet success =
    do hereURL <- showURL here
       formlet <- makeFormlet
       return [multiFormPart formName hereURL success Nothing formlet]
