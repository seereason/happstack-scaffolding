{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-orphans #-}
module Scaffolding.Comment.Types where

import Data.Data       (Data, Typeable)
import Data.SafeCopy   (base, deriveSafeCopy)
import Data.Sequence   (Seq)
import Data.Time       (UTCTime)
import Data.Text       (Text)
import Data.UserId (UserId(..))
import Test.QuickCheck (Arbitrary)
import Web.Routes      (PathInfo)

newtype TextHtml = TextHtml { unTextHtml :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''TextHtml)

newtype CommentId = CommentId { unCommentId :: Integer }
      deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo, Arbitrary, Enum)
$(deriveSafeCopy 0 'base ''CommentId)

data CommentModeration 
    = UnmoderatedComment
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CommentModeration)

data Spaminess 
    = Spaminess { spaminess :: Int
                , isSpam :: Bool
                }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Spaminess)

incSpaminess :: Spaminess -> Spaminess
incSpaminess s = 
    let spaminess'= succ (spaminess s)
    in s { spaminess = spaminess'
         , isSpam = spaminess' >= 10
         }

data Comment 
    = Comment { commentId         :: CommentId
              , commenter         :: UserId
              , commentDate       :: UTCTime
              , commentRaw        :: Text
              , commentHtml       :: TextHtml
              , commentSpaminess  :: Spaminess
              }
      deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Comment)

data CommentList topic
    = CommentList 
      { topic :: topic
      , comments :: Seq Comment
      }
      deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CommentList)
