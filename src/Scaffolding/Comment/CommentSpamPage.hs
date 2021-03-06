{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards,
             ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs -Wall -Wwarn #-}
module Scaffolding.Comment.CommentSpamPage
    ( commentSpamPage
    ) where

import Data.Acid (AcidState)
import Data.Acid.Advanced (update')
import Data.Data (Data)
import Data.SafeCopy (SafeCopy)
import qualified Data.Text.Lazy as TL
import Happstack.Server (Happstack, Response, Method(POST), methodM, ok, ToMessage)
import HSP (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), XML, XMLType, genElement, fromStringLit)
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import Scaffolding.Comment.Acid (State, AcidComment(..), FlagComment(..))
import Scaffolding.Comment.Types (CommentId(..))
import Scaffolding.ProfileData.User (MonadUser)
import Web.Routes.RouteT (MonadRoute, URL)

commentSpamPage :: forall m topic.
                   (Happstack m,
                    MonadRoute m,
                    HasAppConf m,
                    ToMessage (XMLType m),
                    XMLType m ~ XML,
                    MonadUser m,
                    MonadRender m,
                    AcidComment topic m,
                    EmbedAsAttr m (Attr TL.Text (URL m)),
                    Data topic, Ord topic, SafeCopy topic, Show topic) =>
                   CommentId
                -> m Response
commentSpamPage commentId =
    do (acid :: AcidState (State topic)) <- askAcidComment
       methodM POST
       _ <- update' acid (FlagComment commentId)
       ok =<< template "Comment Flagged" ()
                           <div class="column-box">
                             <p>Comment flagged as spam. Thank you!</p>
                           </div>
