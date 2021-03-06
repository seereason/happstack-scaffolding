{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -Wwarn -fno-warn-orphans #-}
module Scaffolding.Comment.Acid
    ( State(..)
    , AcidComment(..)
    , Comments
    , initial
    , AskComment(..)
    , AskComments(..)
    , AskCommentsOn(..)
    , AddComment(..)
    , FlagComment(..)
    ) where

import Control.Applicative     ((<$>))
import "mtl" Control.Monad.Reader    (MonadReader(ask))
import "mtl" Control.Monad.State     (MonadState(get,put))
import Data.Acid               (AcidState, Update, Query, makeAcidic)
import Data.Data               (Data, Typeable)
import Data.Foldable           (toList)
import Data.IxSet ((@=), getOne, {-inferIxSet,-} Indexable, IxSet, ixSet, noCalcs, flattenWithCalcs)
import qualified Data.IxSet    as IxSet
import Data.IxSet.Ix (Ix(Ix))
import Data.List               (find)
import qualified Data.Map as Map
import Data.SafeCopy           (SafeCopy(..), base, contain, getSafeGet, getSafePut, deriveSafeCopy)
import Data.Sequence           ((|>))
import qualified Data.Sequence as Seq
import Data.Serialize (label)
import qualified Data.Set as Set
import Data.UserId (UserId)
import Scaffolding.Comment.Types -- (CommentId, Comments, incSpaminess)

{-

WANTED:

 A rose tree structure where each node can be assigned an Id, and it is quick to find that node. The id should not change as other nodes are added or removed.

Also need to be able to easily insert new children given a reference.

The ordering of the comments in the tree might vary. They could be show in the order posted. But they could also be weighted by voting. So, perhaps only the raw data should be stored, and the various views should be cached ? 

-}

-- $(inferIxSet "Comments" ''CommentList 'noCalcs [''Topic, ''CommentId, ''UserId])

instance (Data topic, Ord topic) => Indexable (CommentList topic) where
    empty = ixSet [Ix (Map.empty :: Map.Map topic     (Set.Set (CommentList topic))) (flattenWithCalcs noCalcs),
                   Ix (Map.empty :: Map.Map CommentId (Set.Set (CommentList topic))) (flattenWithCalcs noCalcs),
                   Ix (Map.empty :: Map.Map UserId    (Set.Set (CommentList topic))) (flattenWithCalcs noCalcs)]

type Comments topic = IxSet (CommentList topic)

data State topic =
    State { commentLists  :: Comments topic
          , nextCommentId :: CommentId
          } deriving (Data, Eq, Ord, Show, Typeable)
#if 0
-- Need Data and Ord supers, see safecopy issue #29.
$(deriveSafeCopy 0 'base ''State)
#else
instance (SafeCopy topic, Data topic, Ord topic) => SafeCopy (State topic) where
      putCopy
        (State a i)
        = contain
            (do { safePut_IxSetCommentListtopictopic_anzo <- getSafePut;
                  safePut_CommentId_anzp <- getSafePut;
                  safePut_IxSetCommentListtopictopic_anzo a;
                  safePut_CommentId_anzp i;
                  return () })
      getCopy
        = contain
            (label
               "Scaffolding.Comment.Acid.State:"
               (do { safeGet_IxSetCommentListtopictopic_anzq <- getSafeGet;
                     safeGet_CommentId_anzr <- getSafeGet;
                     (((return State) <*> safeGet_IxSetCommentListtopictopic_anzq)
                      <*> safeGet_CommentId_anzr) }))
      version = 0
      kind = base
      errorTypeName _ = "Scaffolding.Comment.Acid.State"
#endif

class AcidComment topic m | m -> topic where
    askAcidComment :: m (AcidState (State topic))

initial :: (Data topic, Ord topic) => State topic
initial
    = State { commentLists  = IxSet.empty
            , nextCommentId = CommentId 1
            }

askComments :: Ord topic => Query (State topic) [CommentList topic]
askComments =
    do cls <- commentLists <$> ask
       return (IxSet.toList cls)

askComment :: (Data topic, Ord topic) => CommentId -> Query (State topic) (Maybe Comment)
askComment cid =
    do cls <- commentLists <$> ask
       case getOne $ cls @= cid of
         Nothing   -> return   Nothing
         (Just cl) -> return $ find (\c -> (commentId c) == cid) (toList (comments cl))

askCommentsOn :: (Data topic, Ord topic) => topic -> Query (State topic) (Maybe (CommentList topic))
askCommentsOn x =
    do c <- commentLists <$> ask
       return (getOne (c @= x))

-- FIXME: this let's you comment on stories and prompts which have not been created yet
addComment :: (Data topic, Ord topic) => topic -> Comment -> Update (State topic) (Either String Comment)
addComment coid comment =
    do cs <- get
       let cl = case getOne (commentLists cs @= coid) of
                  Nothing -> CommentList coid Seq.empty
                  (Just c) -> c
           comment'  = comment { commentId = (nextCommentId cs) }
           comments' = (comments cl) |> comment'
           cl' = cl { comments = comments' }
           commentLists' = IxSet.updateIx (topic cl') cl' (commentLists cs)
       put $ cs { commentLists = commentLists'
                , nextCommentId = succ (nextCommentId cs)
                }
       return (Right comment')

-- FIXME: we do not protect against one user flag some as spam multiple times
flagComment :: (Data topic, Ord topic) => CommentId -> Update (State topic) ()
flagComment cid =
    do cs <- get
       case getOne $ (commentLists cs) @= cid of
         Nothing -> return ()
         (Just cl) ->
             case Seq.findIndexL (\c -> commentId c == cid) (comments cl) of
               Nothing -> return ()
               (Just i) ->
                   do let cl' = cl { comments = Seq.adjust (\c -> c { commentSpaminess = incSpaminess (commentSpaminess c) }) i (comments cl) }
                          cs' = cs { commentLists = IxSet.updateIx (topic cl') cl' (commentLists cs) }
                      put cs'

$(makeAcidic ''State
   [ 'askComment
   , 'askComments
   , 'askCommentsOn
   , 'addComment
   , 'flagComment
   ])

