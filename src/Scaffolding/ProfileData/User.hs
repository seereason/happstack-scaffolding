{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, PackageImports,
             RecordWildCards, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}
module Scaffolding.ProfileData.User
    ( MonadUser(askAcidAuth, askAcidProfile)
    , MonadUserName(askAcidProfileData)
    , lookUser
    , lookMaybeUserId
    , userName
    , lookUsername
    , lookUsername'
    ) where

import "mtl" Control.Monad.Trans (MonadIO)
import Data.Acid (AcidState)
import Data.Acid.Advanced (query')
import qualified Data.Text as T
import Happstack.Auth.Core.Auth (AuthState)
import Happstack.Auth.Core.AuthURL (AuthURL(..))
import Happstack.Auth.Core.Profile (UserId(..), getUserId, ProfileState)
import Happstack.Server (Happstack, ToMessage(..), escape, toResponse, internalServerError)
import HSP (XMLGenT(..))
import qualified Scaffolding.ProfileData.Acid as ProfileData
import Scaffolding.ProfileData.URL (MkURL(authURL))
import Web.Routes.Happstack (seeOtherURL) -- ServerMonad, WebWonad, and FilterMonad instances
import Web.Routes.RouteT (MonadRoute(..), URL)
import Web.Routes.XMLGenT () -- ShowURL XMLGenT  instance

class MonadUser m where
    askAcidAuth :: m (AcidState AuthState)
    askAcidProfile :: m (AcidState ProfileState)

class MonadUser m => MonadUserName m where
    askAcidProfileData :: m (AcidState ProfileData.State)

deriving instance MonadUser m => MonadUser (XMLGenT m)
deriving instance MonadUserName m => MonadUserName (XMLGenT m)

-- | Look up the name of the currently logged in user
lookUsername :: (MonadUserName m, MonadRoute m, Happstack m, MkURL (URL m)) => m T.Text
lookUsername = lookMaybeUsername >>= maybe (escape $ seeOtherURL (authURL A_Login)) return

lookMaybeUsername :: (MonadUserName m, MonadRoute m, Happstack m) => m (Maybe T.Text)
lookMaybeUsername = lookMaybeUserId >>= maybe (return Nothing) (\ u -> lookUsername' u >>= return . Just)

-- | Look up the user name associated with a particular user id.
lookUsername' :: (MonadUserName m, MonadRoute m, Happstack m) => UserId -> m T.Text
lookUsername' uid = 
    do e <- userName uid
       case e of
         (Right n) -> return n
         (Left e') -> escape $ internalServerError $ toResponse $ "Internal Server Error - " ++ ProfileData.message e'

userName :: (MonadUserName m, MonadIO m) => UserId -> m (Either ProfileData.Error T.Text)
userName u =
    do acidProfileData <- askAcidProfileData
       query' acidProfileData (ProfileData.UsernameById u)

lookUser :: (MonadUser m, MonadRoute m, Happstack m, MkURL (URL m)) => m UserId
lookUser =
    do m <- lookMaybeUserId
       case m of
         (Just uid) -> return uid
         Nothing -> escape $ seeOtherURL (authURL A_Login)

lookMaybeUserId :: (MonadUser m, Happstack m) => m (Maybe UserId)
lookMaybeUserId = 
    do acidAuth <- askAcidAuth
       acidProfile <- askAcidProfile
       getUserId acidAuth acidProfile
