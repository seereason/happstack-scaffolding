{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Scaffolding.MkURL
       ( URL(..)
       , MkURL(..)
       ) where

import Control.Applicative ((<$>))
import Data.Data
import Happstack.Auth.Core.AuthURL (AuthURL)
import Happstack.Auth.Core.Profile (UserId(..))
import Happstack.Auth.Core.ProfileURL (ProfileURL)
import Test.QuickCheck     (Arbitrary(..),oneof)
import Web.Routes.TH       (derivePathInfo)

data URL
    = CreateNew
    | View UserId
    | EditUserName
    | Edit
    | JSONAllUserIdsAndNames
      deriving (Eq, Ord, Read, Show, Data, Typeable)

class MkURL url where
    mkURL :: URL -> url 
    authURL :: AuthURL -> url
    userURL :: UserId -> url
    profileURL :: ProfileURL -> url

$(derivePathInfo ''URL)
instance Arbitrary URL where
    arbitrary = 
        oneof [ return CreateNew 
              , View . UserId <$> arbitrary
              , return EditUserName 
              , return Edit
              , return JSONAllUserIdsAndNames
              ]
