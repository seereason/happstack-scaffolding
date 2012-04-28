{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -Werror -F -pgmFtrhsx #-}
module Scaffolding.Pages.FormPart
    ( FormDF
    , formPart
    , multiFormPart
    , notEmpty
    , fieldset
    , ol
    , li
    , minLengthString
    , minLengthText
    , nullToNothing
    , rowsToList
    , rowsToColumn
    , submitOnChange
    , seqA
    ) where

import Control.Applicative (Alternative, Applicative(pure), (*>))
import Control.Monad (MonadPlus(mplus), msum)
import Data.List (intersperse)
import Data.Monoid (Monoid(mempty), mconcat)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Traversable (sequenceA)
import Happstack.Server (Happstack, Request(rqMethod), ToMessage(..), Method(..), Input, escape, methodM, method, getDataFn, look, localRq)
import HSP (XMLGenerator, XMLGenT(..), Attr(..), EmbedAsAttr(..), EmbedAsChild(..), SetAttr, GenXML, (<@), genElement, genEElement, unXMLGenT)
import Happstack.Server.HSX       () -- instance (ServerMonad XMLGenT)
import qualified HSX.XMLGenerator as HSX
import Scaffolding.Pages.AppTemplate (MonadRender)
import Text.Digestive ((++>), Validator, Transformer, Form(..), FormRange(..), Result(..), 
                       View(unView), check, mapView, runForm, transformEither, validate, view, viewForm)
import Text.Digestive.Forms.Happstack (happstackEnvironment)
import Text.Digestive.HSP.Html4 (errors)

type FormDF m a = Form m [Input] String [GenXML m] a

-- | turn a formlet into XML+ServerPartT which can be embedded in a larger document
formPart ::
  (EmbedAsChild m xml, EmbedAsAttr m (Attr String String), EmbedAsAttr m (Attr String Text), ToMessage b, Happstack m, Alternative m)
  => String     -- ^ prefix
  -> Text       -- ^ url to POST form results to
  -> (a -> m b) -- ^ handler used when form validates
  -> Maybe ([(FormRange, e)] -> [GenXML m] -> m b) -- ^ handler used when form does not validate
  -> Form m [Input] e xml a      -- ^ the formlet
  -> GenXML m
formPart prefix action handleSuccess mHandleFailure form =
  XMLGenT $ 
    msum [ do methodM [GET, HEAD]
              v <- viewForm form prefix
              unXMLGenT $ 
                <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
                  <% v %>
                 </form>
         , do methodM POST
              (v,r') <- runForm form prefix $ happstackEnvironment
              r <- r'
              case r of
                (Ok a)    -> (escape . fmap toResponse) $ handleSuccess a
                (Error e) ->
                    case mHandleFailure of
                      (Just handleFailure) ->
                          (escape . fmap toResponse) $
                             handleFailure e [ <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
                                                 <% unView v e %>
                                               </form>
                                             ]
                      Nothing ->
                          unXMLGenT $ 
                            <form action=action method="POST" enctype="multipart/form-data" accept-charset="UTF-8">
                             <% unView v e %>
                            </form>
         ]


multiFormPart :: (EmbedAsChild m xml,
                  EmbedAsAttr m (Attr String String),
                  EmbedAsAttr m (Attr String Text),
                  ToMessage b,
                  Happstack m,
                  Alternative m) =>
                 String -- ^ unique name for the formlet
              -> Text -- ^ url to POST form results to
              -> (a -> m b) -- ^ handler used when form validates
              -> Maybe ([(FormRange, e)] -> [GenXML m] -> m b) -- ^ handler used when form does not validate
              -> Form m [Input] e xml a      -- ^ the formlet
              -> GenXML m
multiFormPart name action success failure form = guard name (formPart name (action `Text.append` (Text.pack $ "?form=" ++ name)) success failure form)
    where
      guard :: (Happstack m) => String -> m a -> m a
      guard formName part =
          (do method POST
              submittedName <- getDataFn (look "form")
              if (submittedName == (Right formName))
               then part
               else localRq (\req -> req { rqMethod = GET }) part
          ) `mplus` part


notEmpty :: (Monad m) => Validator m String Text
notEmpty = (check "field can not be empty") (not . Text.null)

fieldset :: MonadRender m => FormDF m a -> FormDF m a
fieldset = mapView $ \xml -> [<fieldset class="top"><% xml %></fieldset>]

ol :: MonadRender m => FormDF m a -> FormDF m a
ol = mapView $ \xml -> [<ol><% xml %></ol>]

li :: MonadRender m => FormDF m a -> FormDF m a
li = mapView $ \xml -> [<li><% xml %></li>]

minLengthText :: MonadRender m => Int -> FormDF m Text -> FormDF m Text
minLengthText 0 f = f
minLengthText 1 f = errors ++> (f `validate` (check "This field can not be empty." (not . Text.null)))
minLengthText n f = errors ++> (f `validate` (check ("This field must be at least " ++ show n ++ " characters.") (\t -> Text.length t >= n)))

minLengthString :: MonadRender m => Int -> FormDF m String -> FormDF m String
minLengthString 0 f = f
minLengthString 1 f = errors ++> (f `validate` (check "This field can not be empty." (not . null)))
minLengthString n f = errors ++> (f `validate` (check ("This field must be at least " ++ show n ++ " characters.") (\t -> length t >= n)))

nullToNothing :: (Monad m) => Transformer m e Text (Maybe Text)
nullToNothing = 
    transformEither $ \t ->
        Right $ if Text.null t
                 then Nothing
                 else (Just t)
          
seqA :: (Functor m, Monad m) => [Form m i e v a] -> Form m i e [v] [a]
seqA xs = sequenceA (map ((: []) `mapView`) xs)
                
rowsToList :: (XMLGenerator x, Monoid b, Functor m, Monad m, EmbedAsChild x c, EmbedAsAttr x (Attr String a)) =>
              a -> [Form m i e c b] -> Form m i e [GenXML x] b
rowsToList class' [] = view [<div class=class'>(none)</div>] *> pure mempty
rowsToList class' rows 
    = (\xs -> [<ul class=class'>
                  <% map (\x -> <li><% x %></li>) xs %>
               </ul>])
      `mapView` (fmap mconcat $ seqA rows)

rowsToColumn :: (EmbedAsChild x [Char], Monoid b, Functor m, Monad m) =>
                [Form m i e [GenXML x] b] -> Form m i e [XMLGenT x (HSX.XMLType x)] b
rowsToColumn [] = view [<span>(none)</span>] *> pure mempty
rowsToColumn rows = (mconcat . intersperse [<hr/>]) `mapView` (fmap mconcat $ seqA rows)

submitOnChange :: (EmbedAsAttr m (Attr String String), SetAttr m elem) => elem -> GenXML m
submitOnChange elm = elm <@ ("onChange" := "submit()")
