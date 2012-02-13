{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, MultiParamTypeClasses, PackageImports,
             RecordWildCards, TypeFamilies #-}
{-# OPTIONS -Wwarn #-}
module Scaffolding.ProfileData.Acid 
       ( -- * Data
         Role(User, Admin)
       , Record(Record, dataFor, username, email, optOut, roles)
       , Error(InvalidUserId)
       , message
       , State(records)
       , initial
       , initialRecord
         -- * Transactions
       , NewRec(..)
       , AskRec(..)
       , HasRole(..)
       , AddRole(..)
       , RemoveRole(..)
       , SetUsername(..)
       , SetEmail(..)
       , SetOptOut(..)
       , UsernameById(..)
       , AllUserIdsAndNames(..)
       ) where

import "mtl" Control.Monad.Reader (ask)
import "mtl" Control.Monad.State (get, put)
import Data.Acid                   (Query, Update, makeAcidic)
import Data.Generics               (Data, Typeable)
import Data.SafeCopy               (base, extension, deriveSafeCopy, Migrate(..))
import qualified Data.Set          as Set
import           Data.Set          (Set)
import Data.Text                   (Text, pack)
import Happstack.Auth.Core.Profile (UserId(..))
import Data.IxSet        ((@=), getOne, inferIxSet, noCalcs)
import qualified Data.IxSet as IxSet

data Role
    = User
    | Admin
      deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''Role)

data Record
    = Record { dataFor  :: UserId
                , username :: Text
                , email    :: Maybe Text
                , optOut   :: Bool
                , roles    :: Set Role
                }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 2 'base ''Record)

data Error
    = InvalidUserId UserId
      deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''Error)

message :: Error -> String
message (InvalidUserId (UserId userId)) = "Could not find any profile data associated with the user id " ++ show userId

$(inferIxSet "Records" ''Record 'noCalcs [''UserId, ''Text])

data State =
    State { records :: Records }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''State)

modifyRecord :: (Record -> Record) -> UserId -> Update State ()
modifyRecord fn uid =
    do ps@(State {..}) <- get
       case getOne $ records @= uid of
         Nothing -> return ()
         (Just profileData) ->
             do let profileData' = fn profileData
                put ps { records = IxSet.updateIx (dataFor $ profileData') profileData' records }

-- | create new profile data
-- 
-- returns: 'Nothing' if 'ProfileData' was already available
-- returns: '(Just profileData)' if new data was created
newRec :: Record -> Update State (Maybe Record)
newRec rec =
    do pds@(State {..}) <- get
       if IxSet.null (records @= (dataFor rec))
          then do put $ pds { records = IxSet.insert rec records }
                  return (Just rec)
          else return Nothing

askRec :: UserId -> Query State (Maybe Record)
askRec uid =
    do State{..} <- ask
       return $ getOne $ records @= uid

hasRole :: UserId -> Role -> Query State Bool
hasRole uid role =
    do mp <- askRec uid
       case mp of
         Nothing -> return False
         (Just profile) ->
             return (role `Set.member` roles profile)
    
addRole :: UserId -> Role -> Update State ()
addRole uid role =
    modifyRecord fn uid
    where
      fn profileData = profileData { roles = Set.insert role (roles profileData) }

removeRole :: UserId -> Role -> Update State ()
removeRole uid role =
    modifyRecord fn uid
    where
      fn profileData = profileData { roles = Set.delete role (roles profileData) }

setUsername :: UserId -> Text -> Update State ()
setUsername uid username =
    modifyRecord fn uid
    where
      fn profileData = profileData { username = username }

setEmail :: UserId -> Maybe Text -> Update State ()
setEmail uid email =
    modifyRecord fn uid
    where
      fn profileData = profileData { email = email }

setOptOut :: UserId -> Bool -> Update State ()
setOptOut uid optOut =
    modifyRecord fn uid
    where
      fn profileData = profileData { optOut = optOut }

usernameById :: UserId -> Query State (Either Error Text)
usernameById uid =
    do State{..} <- ask
       case getOne $ records @= uid of
         Nothing  -> return (Left (InvalidUserId uid))
         (Just (Record{..})) -> return (Right username)

allUserIdsAndNames :: Query State [(UserId, Text)]
allUserIdsAndNames =
    do State{..} <- ask
       return $ map (\Record{..} -> (dataFor, username)) $ IxSet.toList records 

$(makeAcidic ''State 
                [ 'newRec
                , 'askRec
                , 'usernameById
                , 'allUserIdsAndNames
                , 'setUsername
                , 'setEmail
                , 'setOptOut
                , 'hasRole
                , 'addRole
                , 'removeRole
                ]
 )

initial :: State
initial = State { records = IxSet.empty }

initialRecord :: UserId -> Record
initialRecord u = Record u (pack "Anonymous") Nothing False (Set.singleton User)
