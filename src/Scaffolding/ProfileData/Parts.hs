{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Scaffolding.ProfileData.Parts 
       ( handle
       ) where

import Control.Monad.Trans (MonadIO)
import Data.Acid.Advanced (update', query')
import Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Auth.Core.Profile  (UserId(..), getUserId)
import Happstack.Server 
import HSP (EmbedAsAttr, Attr)
import qualified HSX.XMLGenerator as HSX
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Pages.AppTemplate (MonadRender)
import qualified Scaffolding.ProfileData.Acid as ProfileData
import Scaffolding.ProfileData.Pages (editProfileDataPage, editUserNamePage)
import qualified Scaffolding.ProfileData.URL as ProfileData
import Scaffolding.ProfileData.User (MonadUserName, askAcidAuth, askAcidProfile, askAcidProfileData)
import Text.JSON                     (encode)
import Web.Routes.Happstack          (seeOtherURL)
import Web.Routes.RouteT (MonadRoute, URL)

handle :: (Happstack m, MonadRoute m, MonadIO m, MonadUserName m, MonadRender m, HasAppConf m, ProfileData.MkURL (URL m),
           EmbedAsAttr m (Attr String (URL m)), ToMessage (HSX.XML m)) =>
          Text -> ProfileData.URL -> m Response
handle postCreateURL url =
    case url of
      ProfileData.CreateNew ->
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
                        Nothing  -> seeOtherURL (ProfileData.userURL uid)
                        (Just _) -> seeOther postCreateURL (toResponse ())
      (ProfileData.View uid) ->
          do profileDataH <- askAcidProfileData
             mProfileData <- query' profileDataH (ProfileData.AskRec uid)
             ok $ toResponse $ show mProfileData
      ProfileData.EditUserName ->
          do editUserNamePage (ProfileData.mkURL url)
      ProfileData.Edit ->
          do editProfileDataPage (ProfileData.mkURL url)
      ProfileData.JSONAllUserIdsAndNames ->
          do profileDataH <- askAcidProfileData
             uan <- query' profileDataH ProfileData.AllUserIdsAndNames
             setHeaderM "content-type" "application/json"
             ok $ toResponse (asJSON uan)
          where
            asJSON = encode . map (\(UserId u,t) -> (u, Text.unpack t))
