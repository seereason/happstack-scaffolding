{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall -Wwarn #-}
module Scaffolding.ProfileData.Pages
    ( editUserNamePage
    , editProfileDataPage
    , renderUser
    ) where

import Control.Applicative         ((<$>), (<*), (<*>))
import Control.Monad               (when)
import Control.Monad.Trans (MonadIO)
import Data.Acid.Advanced (query', update')
import qualified Data.Set as Set
import Data.Text                   (Text)
import Happstack.Auth.Core.Profile (UserId(..))
import Happstack.Server
import HSP
import qualified HSX.XMLGenerator as HSX
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import Scaffolding.Pages.FormPart (FormDF, formPart, fieldset, li, ol, nullToNothing)
import Scaffolding.Pages.InternalServerError (internalServerErrorPage)
import qualified Scaffolding.ProfileData.Acid as ProfileData
import Scaffolding.ProfileData.URL (MkURL(userURL))
import Scaffolding.ProfileData.User (MonadUserName, askAcidProfileData, lookUser, lookUsername, lookUsername')
import Scaffolding.Session (requireSession)
import Text.Digestive              ((++>), (<++), transform)
import Text.Digestive.Forms (FormInput)
import Text.Digestive.HSP.Html4    (inputCheckBox, inputText, submit, label)
import Text.Digestive.Types (Form)
import Web.Routes.RouteT (MonadRoute, URL)
import Web.Routes                  (showURL)

editProfileDataPage :: forall m. (Happstack m, MonadRoute m, MonadUserName m, MkURL (URL m), MonadIO m, MonadRender m, HasAppConf m, EmbedAsAttr m (Attr String (URL m)), ToMessage (HSX.XML m)) =>
                       URL m -> m Response
editProfileDataPage here =
    requireSession $
      do userId <- lookUser
         mProfileData <- askAcidProfileData >>= \ state -> query' state (ProfileData.AskRec userId)
         case mProfileData of
           Nothing -> internalServerErrorPage ("No profile data found for " ++ show (unUserId userId))
           (Just profileData) ->
               do actionURL    <- showURL here
                  template "edit profile data" () $
                    <div id="content" class="column-box">
                      <h2>Edit Profile Data</h2>
                      <% formPart "pd" actionURL (updateProfileData profileData) Nothing (editProfileDataForm (Just $ ProfileData.username profileData) (ProfileData.email profileData) (ProfileData.optOut profileData) (ProfileData.roles profileData)) %>
                    </div>
    where
      updateProfileData :: ProfileData.Record -> (Text, Maybe Text, Bool, Bool) -> m Response
      updateProfileData profileData (name, newEmail, newOptOut, userFlag) =
          do state <- askAcidProfileData
             -- Right now we only have a control to add or remove the
             -- User role, eventually we would want controls to add
             -- and remove all of the roles.
             let newRoles = (if userFlag then Set.insert ProfileData.User else Set.delete ProfileData.User) (ProfileData.roles profileData)
             when (ProfileData.username profileData /= name)      $ update' state (ProfileData.SetUsername (ProfileData.dataFor profileData) name)
             when (ProfileData.email profileData    /= newEmail)  $ update' state (ProfileData.SetEmail    (ProfileData.dataFor profileData) newEmail)
             when (ProfileData.optOut profileData   /= newOptOut) $ update' state (ProfileData.SetOptOut   (ProfileData.dataFor profileData) newOptOut)
             let addedRoles = Set.toList $ Set.difference newRoles (ProfileData.roles profileData)
             mapM_ (update' state . ProfileData.AddRole (ProfileData.dataFor profileData)) addedRoles
             let removedRoles = Set.toList $ Set.difference (ProfileData.roles profileData) newRoles
             mapM_ (update' state . ProfileData.RemoveRole (ProfileData.dataFor profileData)) removedRoles
             template "profile updated" () $
              <div id="content" class="column-box">
               <p>profile updated.</p>
              </div>

editProfileDataForm :: MonadRender m => Maybe Text -> Maybe Text -> Bool -> Set.Set ProfileData.Role -> FormDF m (Text, Maybe Text, Bool, Bool)
editProfileDataForm username email optOut roles =
    fieldset $ ol $
     ((,,,) <$> (li $ (label "your name: "  ++> inputText username)) 
            <*> (li $ (label "your email: " ++> (inputText email `transform` nullToNothing)))
            <*> (li $ (inputCheckBox optOut <++ label "opt-out of email list."))
            <*> (li $ (inputCheckBox (Set.member ProfileData.User roles)  <++ label "add to User group."))
            <*  (li $ submit "update"))

editUserNamePage :: (Happstack m, MonadRoute m, MonadUserName m, MkURL (URL m), MonadRender m, HasAppConf m, EmbedAsAttr m (Attr String (URL m)), ToMessage (HSX.XML m)) =>
                    URL m -> m Response
editUserNamePage here =
    requireSession $
      do username  <- lookUsername
         actionURL <- showURL here
         template "edit username" () $
          <div id="content" class="column-box">
           <h2>Edit Name</h2>
           <% formPart "e" actionURL updateUsername Nothing (editUserNameForm (Just username)) %>
          </div>
    where
      updateUsername name =
          do uid <- lookUser
             apd <- askAcidProfileData
             _ <- update' apd (ProfileData.SetUsername uid name)
             template "Username updated" () $
              <div id="content" class="column-box">
               <p>Username updated to <% name %></p>
              </div>

editUserNameForm :: (FormInput i f, XMLGenerator x, Functor m, Monad m) =>
                    Maybe Text -> Form m i e [XMLGenT x (HSX.XML x)] Text
editUserNameForm mUsername = 
    (label "your name: " ++> inputText mUsername) <* submit "change name"

renderUser :: forall m.
              (Happstack m,
               MonadUserName m,
               MkURL (URL m),
               EmbedAsAttr m (Attr String (URL m)),
               EmbedAsChild m Text,
               MonadRoute m) =>
             UserId -> XMLGenT m (HSX.XML m)
renderUser user =
    lookUsername' user >>= \ name ->
    <a href=(userURL user :: URL m)><% name %></a>
