{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             RankNTypes, RecordWildCards, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs -Wall -Wwarn #-}
module Scaffolding.Comment.CommentPage
    ( doComment
    , commentBox
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Acid (AcidState)
import Data.Acid.Advanced (query')
import Data.Data (Data)
import qualified Data.Foldable     as F
import Data.Maybe (fromMaybe)
import Data.SafeCopy (SafeCopy)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import qualified Data.Sequence     as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Happstack.Auth.Core.Profile (UserId)
import Happstack.Server (Happstack, Response, ok, notFound, ToMessage)
import HSP (XMLGenerator, GenXML, EmbedAsChild(..), EmbedAsAttr(..), Attr(..), XMLGenT, XMLType, StringType, unXMLGenT, genElement, genEElement, fromStringLit)
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Comment.Acid (State, AcidComment(askAcidComment), AskComment(..), AskCommentsOn(..))
import Scaffolding.Comment.CommentSpamPage (commentSpamPage)
import qualified Scaffolding.Comment.URL as Comment
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import qualified Scaffolding.ProfileData.Acid as ProfileData -- (ProfileData(..), AskProfileData(..))
import Scaffolding.Comment.SubmitComment (submitCommentPage)
import Scaffolding.Comment.Types (Comment(..), CommentId(..), CommentList(..), Spaminess(..), TextHtml, isSpam)
import Scaffolding.ProfileData.User (MonadUserName, askAcidProfileData)
import Scaffolding.TimeExtra (fuzzyDiffTime)
import Text.PrettyPrint (Doc)
import Web.Routes.RouteT (MonadRoute, URL)

doComment :: forall topic m.
             (Happstack m,
              MonadRoute m,
              ToMessage (XMLType m),
              MonadUserName m,
              MonadRender m,
              HasAppConf m,
              AcidComment topic m,
              Comment.MkURL topic (URL m),
              EmbedAsAttr m (Attr TL.Text (URL m)),
              EmbedAsChild m TextHtml,
              Data topic,
              Typeable topic,
              Ord topic,
              SafeCopy topic) =>
             (UserId -> URL m)
          -> URL m
          -> Comment.URL topic
          -> m Response
doComment mkUserURL here url =
    case url of
      (Comment.Comment cid) -> commentPage mkUserURL cid
      (Comment.Submit co) -> submitCommentPage here co
      (Comment.Spam cid) -> commentSpamPage cid

commentXML :: forall m topic.
              (Comment.MkURL topic (URL m),
               MonadUserName m,
               XMLGenerator m,
               StringType m ~ TL.Text,
               EmbedAsChild m TextHtml,
               EmbedAsChild m Text.Text,
               EmbedAsAttr m (Attr TL.Text (URL m)),
               MonadIO m,
               Functor m) =>
              (UserId -> URL m) -> Comment -> GenXML m
commentXML mkUserURL comment =
    do acid <- askAcidProfileData
       n   <- fmap ProfileData.username <$> query' acid  (ProfileData.AskRec (commenter comment))
       now <- liftIO $ getCurrentTime
       c   <- <% commentHtml comment %>
       <li class="comment">

         <div class="person-icon-small">
          <img  src="/theme/icons/person-icon.png" alt="person icon" /><br />
         </div>

         <div class="comment-body">
          <span><a class="person" href=(mkUserURL (commenter comment))><% fromMaybe (Text.pack "Anonymous") n %></a><br /></span>
          <% c %>
          <div class="comment-footer">
             <span><% TL.pack $ fuzzyDiffTime (now `diffUTCTime` (commentDate comment)) %></span>
             <form class="comment-spam" action=(Comment.mkURL (Comment.Spam (commentId comment)) :: URL m) method="POST" enctype="multipart/form-data">
              <span class="dot">·</span>
              <input type="submit" value="spam" />
             </form>

            <span class="dot">·</span>
            <span><a href=(Comment.mkURL (Comment.Comment (commentId comment)) :: URL m)>permalink</a></span>
          </div>
         </div>
--         <img src="/theme/comment-bottom.png" alt="" />
        </li>

commentsXML :: (MonadIO m,
                Functor m,
                MonadUserName m,
                Comment.MkURL topic (URL m),
                -- XMLType m ~ TextHtml,
                XMLGenerator m,
                StringType m ~ TL.Text,
                EmbedAsAttr m (Attr TL.Text (URL m)),
                EmbedAsChild m Text.Text,
                EmbedAsChild m TextHtml) =>
               (UserId -> URL m) -> Maybe (CommentList topic) -> GenXML m
commentsXML _ Nothing = <p>No comments yet.</p> -- FIXME: this is probably an internal error ?
commentsXML mkUserURL (Just (CommentList _commentingOn comments))
    | Seq.null comments = <p>No comments yet.</p>
    | otherwise =
        do cmts <-  mapM (commentXML mkUserURL) (F.toList (Seq.filter (not . isSpam . commentSpaminess) comments))
           <ul id="comments">
            <% cmts %>
            </ul>


commentPage :: (MonadRender m,
                ToMessage (XMLType m),
                StringType m ~ TL.Text,
                MonadUserName m,
                Happstack m,
                MonadRoute m,
                HasAppConf m,
                AcidComment topic m,
                Comment.MkURL topic (URL m),
                EmbedAsAttr m (Attr TL.Text (URL m)),
                EmbedAsChild m TextHtml,
                Data topic,
                Typeable topic,
                Ord topic,
                SafeCopy topic) =>
               (UserId -> URL m) -> CommentId -> m Response
commentPage mkUserURL cid =
    do acid <- askAcidComment
       mComment <- query' acid (AskComment cid)
       case mComment of
         Nothing ->
             do -- cs <- query' acidComment AskComments
                notFound ()
                template "comment id not found" () <p>Invalid comment id: <% TL.pack $ show cid %> </p>
         (Just comment) ->
          do c <- unXMLGenT $ commentXML mkUserURL comment
             ok =<< template (show cid) ()
                 <div class="column-box">
                  <ul id="comments">
                   <% c %>
                  </ul>
                 </div>

commentBox :: forall m topic. (MonadIO m,
                               MonadRender m,
                               MonadUserName m,
                               Comment.MkURL topic (URL m),
                               EmbedAsChild m TextHtml,
                               EmbedAsAttr m (Attr TL.Text (URL m)),
                               Data topic,
                               Ord topic,
                               SafeCopy topic) =>
              AcidState (State topic) -> (UserId -> URL m) -> (topic -> Doc) -> String -> topic -> GenXML m
commentBox acid mkUserURL prettyTopic classes co =
    do comments' <- query' acid (AskCommentsOn co)
       cmtsXML <- commentsXML mkUserURL comments'
       -- classes <- lift $ themeClasses Style.Comments
       <div class="comments">
        <h2 class=(TL.pack classes)><% TL.pack $ pluralize "Comment" (maybe 0 (Seq.length . comments) comments') %> on <% TL.pack $ show $ prettyTopic co %></h2>
        <% cmtsXML %>
        <p><a href=(Comment.mkURL (Comment.Submit co) :: URL m)>add a comment</a></p>
        </div>
    where

      pluralize word 1 = "1 " ++ word
      pluralize word n = show n ++ " " ++ word ++ "s"
