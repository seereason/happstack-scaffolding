{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall -Wwarn #-}
module Scaffolding.Comment.SubmitComment
    ( submitCommentPage
    ) where

import Control.Applicative         ((<$>), (<*))
import Control.Monad.Trans         (MonadIO(liftIO))
import Data.Acid.Advanced (update')
import Data.Data (Data)
import Data.SafeCopy (SafeCopy)
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Happstack.Auth.Core.Profile (getUserId)
import Happstack.Server (Happstack, ToMessage, Response)
import HSP (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), genElement)
import qualified HSX.XMLGenerator as HSX
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Comment.Acid (AddComment(..), AcidComment(..))
import Scaffolding.Comment.Types (Comment(..), CommentId(..), TextHtml(..), Spaminess(..))
import qualified Scaffolding.Comment.URL as Comment
import Scaffolding.Markdown                    (markdown)
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import Scaffolding.Pages.FormPart (formPart, FormDF, fieldset, li, ol)
import Scaffolding.ProfileData.User (MonadUser, askAcidAuth, askAcidProfile)
import Text.Digestive              ((++>), check, transform, transformEitherM, validate)
import Text.Digestive.HSP.Html4    (errors, inputTextArea, submit, setAttrs)
import Web.Routes                  (showURL)
import Web.Routes.Happstack        (seeOtherURL)
import Web.Routes.RouteT (MonadRoute, URL)

submitCommentPage :: (Happstack m,
                      MonadRoute m,
                      MonadUser m,
                      MonadRender m,
                      HasAppConf m,
                      AcidComment topic m,
                      Comment.MkURL topic (URL m),
                      EmbedAsAttr m (Attr String (URL m)),
                      ToMessage (HSX.XMLType m),
                      EmbedAsChild m TextHtml,
                      Data topic,
                      Typeable topic,
                      Ord topic,
                      SafeCopy topic) =>
                     URL m -> topic -> m Response
submitCommentPage here coid =
    do actionURL <- showURL here
       template "Submit Comment" ()
        <div id="submit-comment" class="column-box">
         <% formPart "p" actionURL addComment (Just handleFailure) submitCommentForm %>
         </div>
    where
      addComment comment' =
          do acidComment <- askAcidComment
             _comment <- update' acidComment (AddComment coid comment')
             seeOtherURL (Comment.topicURL coid)
      handleFailure _errs formXML =
          template "Submit a comment - Errors " ()
               <div id="main">
                <h1>Failed to add comment</h1>
                <% formXML %>
               </div>

submitCommentForm :: (Happstack m, MonadRoute m, MonadRender m, MonadUser m) => FormDF m Comment
submitCommentForm =
    fieldset (errors ++> (ol $ msg <* submitBtn))
    `transform`
    (transformEitherM toComment)
    where
      -- validators
      notEmptyText :: (Happstack m, MonadRoute m, MonadRender m) => FormDF m Text -> FormDF m Text
      notEmptyText f = errors ++> (f `validate` (check "surely you have more to say than that.") (not . Text.null))

      -- fields
      msg       = li $ notEmptyText $ Text.pack <$> inputTextArea (Just 80) (Just 20)  Nothing
      submitBtn = li $ submit "Submit Comment" `setAttrs` [("class" := "submit")]

      -- transformer
      toComment :: (Happstack m, MonadRoute m, MonadRender m, MonadUser m) => Text -> m (Either String Comment)
      toComment txt =
          do acidAuth <- askAcidAuth
             acidProfile <- askAcidProfile
             mu <- getUserId acidAuth acidProfile
             case mu of
               Nothing -> return (Left "You are not logged in.")
               (Just uid) ->
                   do r <- liftIO $ markdown txt
                      case r of
                        (Left e) -> return (Left $ Text.unpack e)
                        (Right m) ->
                            do now <- liftIO $ getCurrentTime
                               return (Right (Comment { commentId         = CommentId 0
                                                      , commenter         = uid
                                                      , commentDate       = now
                                                      , commentRaw        = txt
                                                      , commentHtml       = TextHtml m
                                                      , commentSpaminess  = Spaminess 0 False
                                                      }))
