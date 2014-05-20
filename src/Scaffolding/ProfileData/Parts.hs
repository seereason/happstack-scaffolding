{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Scaffolding.ProfileData.Parts
       ( handle
       ) where

import Control.Monad.Trans (MonadIO)
import Data.Acid.Advanced (update', query')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Happstack.Auth.Core.Profile  (UserId(..), getUserId)
import Happstack.Server
import HSP (EmbedAsAttr, Attr, XMLType, XMLGenT)
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Pages.AppTemplate (MonadRender)
import qualified Scaffolding.ProfileData.Acid as ProfileData
import Scaffolding.ProfileData.Pages (editProfileDataPage, editUserNamePage)
import qualified Scaffolding.MkURL as MkURL
import Scaffolding.ProfileData.User (MonadUserName, askAcidAuth, askAcidProfile, askAcidProfileData)
import Text.JSON                     (encode)
import Web.Routes.Happstack          (seeOtherURL)
import Web.Routes.RouteT (MonadRoute, URL)

handle :: (Happstack m, MonadRoute m, MonadIO m, MonadUserName m, MonadRender m, HasAppConf m, HasAppConf (XMLGenT m), MkURL.MkURL (URL m),
           EmbedAsAttr m (Attr TL.Text (URL m)), ToMessage (XMLType m)) =>
          Text -> MkURL.URL -> m Response
handle postCreateURL url =
    case url of
      MkURL.CreateNew ->
          do authH <- askAcidAuth
             profileH <- askAcidProfile
             mUserId <- getUserId authH profileH
             case mUserId of
               Nothing -> internalServerError $ toResponse $ "not logged in."
               (Just uid) ->
                   do let rec = ProfileData.initialRecord uid
                      profileDataH <- askAcidProfileData
                      mpd <- update' profileDataH (ProfileData.NewRec rec)
                      case mpd of
                        Nothing  -> seeOtherURL (MkURL.userURL uid)
                        (Just _) -> seeOther postCreateURL (toResponse ())
      (MkURL.View uid) ->
          do profileDataH <- askAcidProfileData
             mProfileData <- query' profileDataH (ProfileData.AskRec uid)
             ok $ toResponse $ show mProfileData
      MkURL.EditUserName ->
          do editUserNamePage (MkURL.mkURL url)
      MkURL.Edit ->
          do editProfileDataPage (MkURL.mkURL url)
      MkURL.JSONAllUserIdsAndNames ->
          do profileDataH <- askAcidProfileData
             uan <- query' profileDataH ProfileData.AllUserIdsAndNames
             setHeaderM "content-type" "application/json"
             ok $ toResponse (asJSON uan)
          where
            asJSON = encode . map (\(UserId u,t) -> (u, Text.unpack t))
