{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, RankNTypes, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances, TypeFamilies #-}
{-# OPTIONS -F -pgmFhsx2hs -Wwarn -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-unused-matches #-}
module Scaffolding.HSP.Widget where

import Control.Applicative ((<$>), Applicative)
import Control.Monad ({-liftM-})
import Control.Monad.RWS (RWS, runRWS, mapRWS)
import Control.Monad.State (MonadState (get, put))
import Control.Monad.Writer (tell)
import "mtl" Control.Monad.Identity (Identity(runIdentity))
import Data.Maybe (fromMaybe)
--import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import HSP.Monad
import HSP.XMLGenerator -- (XMLGenT, XMLGenerator, XMLGen, SetAttr, XMLType, AppendChild, EmbedAsChild, EmbedAsAttr, Attr, StringType, GenXML)
-- import HSP.HTML4 (renderAsHTML)
import qualified HSP.XML as HSP
import HJScript (HJScript', JShow, HasConstructor, Array(Array), Exp(..), HJScript, IsClass, JBool, JInt, JObject, JString, JType, Var, Object(..), Rec, ( # ), (#!), (.=.), (.-.), (.+.), (?), (.==.), first, second, call, callMethod, callVoidMethod, evalHJScript, false, function, functionDecl, inVar, jShow, new, push, string, this, derefVar, procedureDecl, val, postinc, int, varWith, propertyVar, forIn, arrLength, delete, procedure, deref, true, false, foreach, for, outputBlock, jshow, runHJScript, doIfNoElse)
-- import HJScript.DOM ({-alert, document, window-})
import HJScript.XMLGenerator (fromStringLit)
import HJScript.DOM hiding (Object, Form)
--import HJScript.Lang (doIf)
import HJScript.Objects.JQuery (JQuery, append, jSetText, jVal, selectExpr, runExp)
-- import qualified XMLGenerator as HSX
import Happstack.Server (ServerPartT)
import HSP.ServerPartT ()
import Scaffolding.HJScriptExtra
import Scaffolding.MonadStack.Headers (MonadHeaders)
import Text.JSON (JSON, Result(Ok, Error), decode {-, encode-})
import Text.Digestive (Form, transform, transformEither, mapView)
import Text.Digestive.Forms (FormInput, inputString)

class (XMLGenerator x,
       XMLGen x,
       SetAttr x (XMLType x),
       AppendChild x (XMLType x),
       EmbedAsChild x TL.Text,
       EmbedAsChild x Text,
       EmbedAsChild x Char,
       EmbedAsAttr x (Attr TL.Text TL.Text),
       EmbedAsAttr x (Attr TL.Text Int),
       EmbedAsAttr x (Attr TL.Text Bool),
       EmbedAsChild x HSP.XML,
       MonadHeaders IO x,
       StringType x ~ TL.Text
       ) => WidgetGenerator x

-- this is pretty wacky
flattenForm :: Monad m => Form m i e ([HJScript ()], [XMLGenT x (XMLType x)]) a -> Form m i e [XMLGenT x (XMLType x)] a
flattenForm =
    mapView $ \(js, h:hs) ->
        ({-tellHeaders [onReadyXML' js mempty] >>-} h) : hs

{-

We currently do not use any output/input queues. Events are sent one at a time and processed immediately. However, this can cause events to get lost. For example, if a widget sends events as part of initialization, the listener may not be connected yet and those events will be lost. We could instead have a queue that holds the events until someone listens. Then we run the problem of a space leak if no one ever listens.

Really Quick Summary:

A widget is a bundle of HTML and javascript code. The html is rendered
in-place. The javascript code is added to the $(document).onready()
<script> element in the head.

Widgets can communicate with each other by using a simple message
passing system. A widget can create an unlimited number of inputs and
outputs. Each input and output is typed to indicate if it is an input
or and output, and also what type of value it sends/receives.

inputs and outputs can be connected via the 'connect' function. Each
input/output can have multiple connections.

In progress (not that anything is done) is a system for sending values
between Haskell and javascript. It is based around JSON. In theory,
the Haskell code works with nice Haskell values (not low-level json),
and the javascript works with JSON objects. And there is some
non-magic code that converts data from one format to another.

By non-magic, I mean, you have to write that code by hand. This is
because the data formats used by both systems do a poor job of
modelling each other. So, you don't want to work with a JSON value
embedded in Haskell, or a Haskell algebraic type embedded in
javascript. Instead you want two different types which contain the
same information, but are structured ideally for the language they are
used in. At least that is the current hypothesis.

-}

-- sending an event between two widgets requires two triggers.
-- Calling sendMsg will trigger the connect handler
-- the connect handler then triggers the receivers handler.
--
-- it seems like we could just have the sender trigger the reciever
-- directly. But that does not work so nicely in practice. The call to
-- sendMsg needs to know the name of the destination it is sending
-- to. But this is not known until connect is called.


{-

The Id system is for generating values for use as html id
attributes. This is somewhat tricky because some of the ids are
generated in by the haskell code, and some ids are generating by the
javascript code.

We could try to use ajax callsbacks to generate the ids in javascript,
but that seems like it could be rather slow.

Instead we use two different name spaces to ensure the ids generated
in haskell vs javascript are unique, and let each environment keep
it's own unique counter for generating new ids.

It is easy to pass an id from Haskell to javascript. But we can not
really go the other way. That can be a bit troublesome at times.

-}

data Src = Haskell | Javascript deriving Show

data Id     = Id { idSrc :: Src
                 , prefix :: String
                 , unId :: Exp Int }
            | GenId JString

instance Show Id where
    show (Id src prefix (JInt i)) = show src ++ "_" ++ (escape prefix) ++ "_" ++ show i -- this can fail sometimes, because, Exp Int could be something besides a (JInt 1) literal.
        where escape [] = []
              escape ('[' : cs) = '_' : escape cs
              escape (']' : cs) = '_' : escape cs
              escape (c   : cs) =  c  : escape cs
    show _ = error "Show Id"


-- | 'Widgets' have inputs and outputs which are identified by their
-- ids. However, we do not want to mix up inputs and outputs, so we
-- will use some newtype wrappers to mark them.
newtype Out a  = Out Id deriving (Show)
newtype In a   = In Id  deriving (Show)

inToOut :: (In a) -> (Out a)
inToOut (In a) = (Out a)

outToIn :: (Out a) -> (In a)
outToIn (Out a) = (In a)

instance JShow Id where
  jshow (Id src prefix i) = (string ((show src) ++ "_" ++ prefix ++ "_")) .+. jshow i
  jshow _ = error "JShow Id"

{-
instance EmbedAsAttr (ServerPartT IO) (Attr String Id) where
  asAttr (n := i) = asAttr (n := show i)

instance EmbedAsAttr (ServerPartT IO) (Attr TL.Text Id) where
  asAttr (n := i) = asAttr (n := (TL.pack $ show i))
-}


{-
instance EmbedAsAttr Identity (Attr String Id) where
  asAttr (n := i) = asAttr (n := show i)

instance EmbedAsAttr Identity (Attr String (In a)) where
  asAttr (n := i) = asAttr (n := show (getId i))
-}
class GetId a where
  getId :: a -> Id

instance GetId (Out a) where
  getId (Out i) = i

instance GetId (In a) where
  getId (In i) = i

instance GetId Id where
  getId i = i

{-

This is the core type used by the widget system. It is a simple wrapper around the RWS monad.

The reader environment is currently unused.

The writer portion hold the tuple, ([HJScript ()],[XMLGenT m (XMLType
m)]). The first argument is javascript code that needs to be added to
the onReady() function. The second argument is an html generated by
the widget.
The state widget is the tuple (Id, Int). The Id is used to generate
fresh ids in Haskell land. The Int argument is used by hjscript to
generate fresh variable names.

-}

newtype Widget m a =
  Widget { unWidget :: RWS () ([HJScript ()],[GenXML m]) (Id, Int) a }
  deriving (Applicative, Functor, Monad, MonadState (Id, Int))

-- This is just a 'class alias' so that we can have short type signatures
-- class (Functor m, XMLGenerator m, EmbedAsAttr m (Attr TL.Text Id), EmbedAsAttr m (Attr TL.Text TL.Text), (NextId (Widget m)), StringType m ~ TL.Text) => Widgets m
-- instance Widgets Identity
-- instance Widgets HJScript'
-- instance Widgets (ServerPartT IO)

-- | run a 'Widget' and get back the javascript and html it produces
runWidget :: String -> Int -> Widget m a -> (a, ([HJScript ()],[GenXML m]))
runWidget prefix i (Widget rws) =
  let (a, _s, w) = runRWS rws () (Id Haskell prefix (int i), i)
  in (a, w)


--  lift 'mapRWS' to the 'Widget' monad
mapWidget :: ((a, (Id, Int), ([HJScript ()], [GenXML m])) -> (b, (Id, Int), ([HJScript ()], [XMLGenT n (XMLType n)]))) ->
             Widget m a ->
             Widget n b
mapWidget f (Widget w) = Widget $ mapRWS f w

{-
-- | debug tool. show the html+javascript generated by the Widget
showWidget :: Widget Identity a -> String
showWidget widget =
  let (_a, (js, xml)) = runWidget "prefix" 0 widget
      xmlStr = unlines $ map (TL.unpack . renderAsHTML) (map (runIdentity . unXMLGenT) xml)
      jsStr = unlines (map (show . snd . evalHJScript) js)
  in jsStr ++ xmlStr

-- | debug tool. calls 'showWidget' and does 'putStrLn' on the 'String' it return
printWidget :: Widget Identity a -> IO ()
printWidget = putStrLn . showWidget
-}

-- | turn some html into a 'Widget'
html ::  GenXML m -> Widget m ()
html xml =
  Widget $ tell ([], [xml])

-- | turn some javascript into a 'Widget'
js :: HJScript a -> Widget m a
js hjs =
  Widget $ do (i, hjstate) <- get
              let (t, hjstate', block) = runHJScript hjs hjstate
              put (i, hjstate')
              tell ([outputBlock block], [])
              return t

-- | transform the html generated by a widget. Similar to 'plug' from the formlets library.
plugW :: ([GenXML m] -> [GenXML m]) -> Widget m a -> Widget m a
plugW f = Widget . (mapRWS $ \(a, s, (js, xml)) -> (a, s, (js, f xml))) . unWidget


-- | class for generating ids
-- not quite sure if this is useful
class NextId m where
  nextId :: m Id

instance NextId (Widget Identity) where
  nextId = nextId'

instance NextId (Widget (ServerPartT IO)) where
  nextId = nextId'

instance NextId (Widget HJScript') where
  nextId =
    js $ do v <- inVar jNextId
            return (Id Javascript "js" v)


-- | 'Widget' which returns the next unused Haskell Id
nextId' :: Widget m Id
nextId' =
  do ~(v@(Id Haskell prefix (JInt i)), hjstate) <- get
     put (Id Haskell prefix (JInt (succ i)), hjstate)
     return v

-- | gets the next free javascript id
jNextId :: Exp Int
jNextId = call (JConst "nextId") ()

-- | gets the next free javascript id. Also returns a version of the
-- Id that we can use in the Haskell code.. provided we don't try to
-- actually find the number associated with the Id.
jNextId' :: HJScript (Id, Exp Int)
jNextId' =
  do v <- inVar jNextId
     return (Id Javascript "js" v, v)
{-
nextId' :: Widget m Id
nextId' =
  do i <- js $ return jNextId
     return (Id i)
-}

-- * Widget things

-- | get the element (as a JQuery object) which has the specified Id
selectId :: (GetId i) => i -> JObject JQuery
selectId i   = selectExpr (string $ ("#" ++ show (getId i)))


-- | define a new javascript object 'JWidget', which is a Widget which can be created dynamically by the client in javascript.
data JWidget a = JWidget deriving Show
instance IsClass (JWidget a)
instance HasConstructor (JWidget a) () ()
instance HasConstructor (JWidget a) (Exp ElementNode, JInt) (ElementNode, Int)
instance JShow (JWidget a) where
  jshow widget = string "jwidget"


-- | find the 'event' associated with an id
id2event :: (GetId i) => i -> Exp String
id2event i = id2event' (getId i)

id2event' :: Id -> Exp String
id2event' (Id src prefix i) = (string ("sendMsg_" ++ show src ++ "_" ++ prefix ++ "_")) .+. (jShow i)
id2event' (GenId i) = (string ("sendMsg_") .+. i)


-- | listen for events/msgs sent to the specified input
bindId :: (JType a) => In a -> ((Exp JEvent, Exp a) -> HJScript JBool) -> HJScript ()
bindId iId callback =
  do cb <- function callback
     runExp $ selectExpr (document) # bind (id2event (getId iId), cb)
     return ()

-- | connect a widget output to a widget input specifically, when the
-- 'Widget' providing 'Out a' calls 'sendMsg', that message should be
-- delivered to the 'Widget' which providse 'In a'.
--
-- An output can be connected to multiple inputs. And inputs can
-- receive messages from multiple outputs.
connect :: forall a m. (JType a, Show a) => (Out a) -> (In a) -> Widget m ()
connect out inp = js $ jConnect out inp


-- | a version of 'connect' which can be called in javascript code
jConnect :: forall a. (JType a, Show a) => (Out a) -> (In a) -> HJScript ()
jConnect (Out oId) (In iId@(Id src prefix i)) =
       do trigger <- function $ \(event :: Exp JEvent, msg :: Exp a) ->
            do args <- new Array ()
               args # push msg
               runExp $ selectExpr(document) # triggerHandler (id2event iId, args)
               return false
          runExp $ selectExpr (document) # bind (id2event oId .+. (string ".") .+. string (show src ++ "_" ++ prefix ++ "_") .+. (jShow i), trigger)
jConnect _ _ = error "jConnect"

-- | causes the second output to send any values produced by the first
-- output this is used when we embed a widget inside another widget,
-- and we want the messages from the inner widget to automatically be
-- sent by the outer widget.
jResend :: forall a. (JType a, Show a) => (Out a) -> (Out a) -> HJScript ()
jResend out1 (Out out2) = jConnect out1 (In out2)

-- | similar to 'jResend' but for copying incoming messages to the inner 'Widget'.
jReRecv :: forall a. (JType a, Show a) => (In a) -> (In a) -> HJScript ()
jReRecv (In in1) in2 = jConnect (Out in1) in2

-- | disconnect to 'Widgets'. Can be called from javascript.
jDisconnect :: (Out a) -> (In a) -> HJScript ()
jDisconnect (Out oId) (In (Id src prefix i)) =
  runExp $ selectExpr (document) # unbind (id2event oId .+. (string ".") .+. string (show src ++ "_") .+. (string $ prefix ++ "_") .+. (jShow i))
jDisconnect _ _ = error "jDisconnect"

-- | send the 'msg' to the 'Out a'. This will cause any 'In a' which
-- have been 'connect'ed to receive the message.
sendMsg :: Out a -> Exp a -> HJScript ()
sendMsg (Out oId) msg =
  do args <- new Array ()
     args # push msg
     runExp $ selectExpr(document) # triggerHandler (id2event oId, args)


-- | a 'Widget' which applies a javascript function to its input and send the result to its output.
--
-- similar in concept to 'fmap'. See also, 'widgetMap'
-- NOTE TO SELF: this is almost an arrow, except we have, Exp (a -> b) instead of a -> b
widgetMap_ :: (NextId (Widget m), JType a) => Exp (a -> b) -> Widget m (In a, Out b)
widgetMap_ f =
    do iId      <- In  <$> nextId
       oId      <- Out <$> nextId
       js $ bindId iId $ \(_, a) -> do sendMsg oId (call f a) ; return false
       return (iId, oId)

-- | a 'Widget' which applies a javascript function to its input and send the result to its output.
--
-- This just calls 'widgetMap_', but has a different type siganture which is sometimes more convenient.
--
-- similar in concept to 'fmap'. See also, 'widgetMap_'
widgetMap :: (NextId (Widget m), Show a, JType a) => Exp (a -> b) -> (Out a) -> Widget m (Out b)
widgetMap f oIda =
    do (iIda, oIdb) <- widgetMap_ f
       connect oIda iIda
       return oIdb



-- | this 'Widget' listens to multiple 'Widgets' and copies a message
-- sent by any of those 'Widgets' to it's output. Effectively merges
-- all their output into a single output.
mergeOuts :: forall a m. (JType a, NextId (Widget m)) => [Out a] -> Widget m (Out a)
mergeOuts outs =
  do outId <- nextId
     js $ do trigger <- function $ \(event :: Exp JEvent, msg :: Exp a) ->
               do args <- new Array ()
                  args # push msg
                  runExp $ selectExpr(document) # triggerHandler (id2event outId, args)
                  return false
             mapM_ (\(Out oId) -> runExp $ selectExpr (document) # bind (id2event oId, trigger)) outs
             return ()
     return (Out outId)


-- * Some Widgets

-- | create a 'submit' button
--
-- The argument is the label to show in the button. It will also be
-- the String in the Out message.
submit :: (StringType m ~ TL.Text, EmbedAsAttr m (Attr TL.Text TL.Text), EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m)) =>
          String -> Widget m (Out String)
submit val =
  do oId <- Out <$> nextId
     html $ let fromStringLit = HSP.fromStringLit in <input type="submit" id=(getId oId) value=(TL.pack val) />
     js $ do clickCB <- function $ \(jEvent :: JObject JEvent) ->
               do -- jEvent # jPreventDefault ()
                  o <- inVar (selectExpr jThis)
                  sendMsg oId (o # jVal)
                  return false
             runExp $ selectId (getId oId) # click clickCB
     return oId

-- | create a dropdown menu.
--
-- The first argument of the tuple is the
-- label to show in the dropdown. The second argument is the message
-- to send when the item is selected.
select :: (StringType m ~ TL.Text, EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m)) => String -> [(String, Exp a)] -> Widget m (Out a)
select title vals =
  do oId <- Out <$> nextId
     html $ let fromStringLit = HSP.fromStringLit in <select id=(getId oId) />
     js $ do selectElem <- inVar $ selectId (getId oId)
             -- empty elem at the top
             empty <- <option><% title %></option>
             jEmpty <- inVar $ selectExpr empty
             runExp $ jEmpty # jSetData ("val", string "")
             runExp $ selectElem # append empty
             -- remaining elements
             let optionElem (lbl, val) =
                     do e <- <option><% lbl %></option>
                        j <- inVar $ selectExpr e
                        runExp $ j # jSetData ("val", val)
                        runExp $ selectElem # append e
             mapM_ optionElem vals
             changeCB <- function $ \(jEvent :: JObject JEvent) ->
               do elm <- inVar (selectExprContext (string "option:selected", jThis))
--                  v   <- inVar $ (selectExprContext (string "option:selected", jThis)) # (jData "val")
                  doIfNoElse (JNot $ elm .==. jEmpty)
                    ( do sendMsg oId (elm # jData "val")
                         runExp $ selectElem # jSetVal (string "0")
                    )
                  return false
             runExp $ selectId (getId oId) # change changeCB
     return oId

-- | a helper function for creating a widget which response to an incoming event by producing an output string.
input' :: (NextId (Widget m))
          => (Id -> GenXML m) -- ^ takes a unique id and generates the HTML
          -> (Id -> HJScript (Exp (() -> String))) -- ^ takes the same unique id and returns a javascript function which can be used to extract the string value of the widget
          -> Widget m (In String, Out String)
input' genHtml getString =
  do iId <- In  <$> nextId
     oId <- Out <$> nextId
     html $ genHtml (getId iId)
     js $ bindId iId $ \(event :: JObject JEvent, _msg :: JString) ->
             do fn <- getString (getId iId)
                sendMsg oId (call fn ())
                return false
     return (iId, oId)

-- | a text input field widget
-- TODO: pressing enter should submit widget ?
input :: (NextId (Widget m), XMLGenerator m, EmbedAsAttr m (Attr String Id), EmbedAsAttr m (Attr String String), EmbedAsAttr m (Attr String a), StringType m ~ String)
         => a -- ^ initial value of the text field
         -> Widget m (In String, Out String)
input v =
  do input'
        (\i -> <input type="text" id=i value=v />)
        (\i -> function $ \() ->
            do e <- inVar (selectId i)
               return (e # jVal))

-- | a text area widget
textarea :: (XMLGenerator m, EmbedAsAttr m (Attr String Id), EmbedAsChild m a, NextId (Widget m), StringType m ~ String)
            => a  -- ^ initial value of the text field
            -> Widget m (In String, Out String)
textarea v =
  do input'
        (\i -> <textarea id=i><% v %></textarea>)
        (\i -> function $ \() ->
            do e <- inVar (selectId i)
               return (e # jVal))


-- | A Widget which receives a message and shows it in a popup box (i.e, window.alert(msg)).
eventAlert :: forall a m. (JType a, JShow a, NextId (Widget m)) => Widget m (In a)
eventAlert =
  do i <- nextId
     js $ do popup <- function $ \(event :: Exp JEvent, msg :: Exp a) ->
               do window # alert (string "eventAlert: " .+. (jShow msg))
                  return false
             runExp $ selectExpr(document) # bind (id2event i, popup)
     return (In i)

-- | A widget which listens for a message and writes a string representation of it to a <p> tag.
output :: forall a m. (JType a, JShow a, XMLGenerator m, EmbedAsAttr m (Attr String Id), NextId (Widget m), StringType m ~ String) => Widget m (In a)
output =
  do i <- In <$> nextId
     html $ <p id=(getId i) />
     js $ do bindId i $ \(event, str :: Exp a) ->
               do e <- inVar $ selectId i
                  runExp $ e # jSetText (jShow str)
                  return false
     return i


-- * Some JWidgets

-- | a submit button 'JWidget'. See 'jSubmit'.
jSubmit' :: JString -> HJScript (Exp (JWidget (Out String)))
jSubmit' =
  \str ->
    do -- window # alert (string "jSubmit'")
       (jid, jidVal) <- jNextId'
       e <- <input type="submit" id=jid value=str />
       clickCB <- function $ \(jEvent :: JObject JEvent) ->
         do sendMsg (Out jid) str
            return false
       runExp $ selectExpr(e) # click clickCB
       new JWidget (e, jidVal)

-- | a text-input 'JWidget'. See 'jInput'
jInput' :: JString -> HJScript (Exp (JWidget (Out String)))
jInput' =
  \str ->
    do (jid, jidVal) <- jNextId'
       e <- <input type="text" id=jid value=str />
       changeCB <- function $ \(jEvent :: JObject JEvent) ->
         do sendMsg (Out jid) ((selectExpr e) # jAttr "value")
            return false
       runExp $ selectExpr(e) # change changeCB
       new JWidget (e, jidVal)

-- | a submit button 'JWidget'
jInput :: Exp (String -> JWidget (Out String))
jInput = JConst "jInput"

-- | a text input field 'JWidget'
jSubmit :: Exp (String -> JWidget (Out String))
jSubmit = JConst "jSubmit"

-- | the paragraph 'Widget' is a high-level widget which displays a sentence. Clicking on a word which toggle the word on/off. When the word is toggled, a messages is sent which indicates if it was toggled on/off, the text of the word, and the index of the word in the sentence.

data JToggle a = JToggle deriving Show
instance IsClass (JToggle a)
instance HasConstructor (JToggle a) (JBool, Exp a) (Bool, a)
instance JShow (JToggle a) where
  jshow jtoggle = string "jtoggle"

toggled :: JObject (JToggle a) -> JBool
toggled = deref "toggled"

msg :: JObject (JToggle a) -> Exp a
msg     = deref "msg"


-- | creates a single word which can be toggled on/off
-- instead of sticking (bool, string) in an array, should we have a toggle object?
-- toggleWord :: forall a m. (XMLGenerator m, EmbedAsChild m String, EmbedAsAttr m (Attr String Id), EmbedAsAttr m (Attr String String), NextId (Widget m), StringType m ~ String) => String -> JInt -> Widget m (In (JToggle Int), Out (JToggle (Rec Int String)))
toggleWord txt value =
  do oId <- nextId
     iId <- nextId
     html $ let fromStringLit = HSP.fromStringLit in <span id=oId class="clickable"><% TL.pack txt %></span>
     js $ do -- handle output
             clickCB <- function $ \(jEvent :: JObject JEvent) ->
              do e    <- inVar (selectExpr jThis)
                 (e # jData "toggled" ?
                    ( do runExp $  e # jSetCss  ("background", "white")
                         runExp $ (e # jSetData ("toggled"   , false))
                    , do runExp $  e # jSetCss  ("background", "yellow")
                         runExp $ (e # jSetData ("toggled"   , true))))
                 tog  <- new JToggle ((e # jData "toggled") :: JBool, (JRec value (string txt))) :: HJScript (Exp (JToggle (Rec Int String)))
                 args <- new Array ()
                 args # push tog
                 runExp $ selectExpr(document) # triggerHandler (id2event oId, args)
                 return false
             e <- inVar $ selectId oId
             runExp $ e # jSetData ("toggled", false)
             runExp $ e # click clickCB
             -- handle input
             incomingCB <- function $ \(event :: Exp JEvent, jToggle :: JObject (JToggle ())) ->
              do e <- inVar $ selectId oId
                 jToggle # toggled ?
                  ( do runExp $ e # jSetCss ("background", "yellow")
                  , do runExp $ e # jSetCss ("background", "white")
                  )
                 runExp $ e # jSetData ("toggled", jToggle # toggled)
                 return false
             runExp $ selectExpr (document) # bind (id2event iId, incomingCB)
     return (In iId, Out oId)

-- TODO:
-- the paragraph widget gets a list of words to turn on by word number
-- but right now the <spans> are not addressable by word number, only id

{-

Functionality:

 Get's a list of paragraphs. Paragraphs are lists of words. Each word can be toggled on and off.

 Toggling happens two ways:
   1. the user clicks on the word
   2. a signal comes in requesting the word to be toggled

 The widget needs to output units that are useful to the backend. It
also needs to be able to receive those units and figure out what words
to toggle on/off.


to toggle words on/off we need to be able to address them
individually. This means they need to be unique elements in the DOM
tree. The easiest way to do that is to make each word a span.

next we need a way to associate a value with each span which can be
returned when it is clicked.


-}
-- paragraphWidget :: (XMLGenerator m, EmbedAsChild m String, EmbedAsAttr m (Attr String Id), EmbedAsAttr m (Attr String String), NextId (Widget m), StringType m ~ String) => [String] -> Widget m ((In (Array Int)), Out (JToggle (Rec Int String)))
paragraphWidget words =
  do iId <- In <$> nextId
     pId <- nextId
     wordz <- let fromStringLit = HSP.fromStringLit in
              plugW (\c -> [<p class="document" id=pId><% c %></p>])
               (mapM (\(txt, value) -> toggleWord txt value) (zip words (map int [0..])))
     oId <- mergeOuts (map snd wordz)
     js $ bindId iId $ \(event, onWords :: Exp (Array Int)) ->
            do ids <- listArray (map (jshow . getId . fst) wordz)
               setToggle <- function $ \(index :: Exp Int , elem :: Exp ElementNode) ->
                   do (val (onWords #! (int 0)) .==. index) ?
                       ( do jt <- new JToggle (true, index) :: HJScript (Exp (JToggle Int))
                            i <- inVar $ val $ ids #! index
                            sendMsg (Out (GenId i)) jt
                            runExp $ onWords # shift
                            return ()
                       , do jt <- new JToggle (false, index) :: HJScript (Exp (JToggle Int))
                            i <- inVar $ val $ ids #! index
                            sendMsg (Out (GenId i)) jt
                            return () )
                      return true
               runExp $ selectExprContext (string "span", selectId pId) # jEach setToggle
               return false
     return (iId, oId)

-- | a simple 'Widget' which listens for incoming html ElementNodes,
-- wraps them in <li> and adds them to a list. Includes a button to
-- delete an element from the list, though the button is not hooked up
-- at the moment.
--
-- This function is obsolete.
{-
dynListWidget :: (Functor m, XMLGenerator m, EmbedAsAttr m (Attr String Id), EmbedAsAttr m (Attr String String), NextId (Widget m), StringType m ~ String) => Widget m (In ElementNode)
dynListWidget =
  do iId  <- In <$> nextId
     ulId <- nextId
     html $ <ol id=ulId class="dynList"></ol> -- technically invalid
     h <- js $ do bindId iId $ \(event, elem :: Exp ElementNode) ->
                    do e <- inVar $ selectId ulId
                       li <- <li><% elem %><input type="submit" value="x" /></li>
                       runExp $ e # append(li)
                       return false
                  <ol id=ulId class="dynList" />
     return iId
-}
-- | A 'widget' which listens for a 'String' and sends an <li> element which contains that string
listItem :: (NextId (Widget m)) => Widget m (In String, Out ElementNode)
listItem =
  do iId <- In  <$> nextId
     oId <- Out <$> nextId
     h <- js $ do bindId iId $ \(event, str :: JString) ->
                    do li <- <li><% str %></li>
                       sendMsg oId li
                       return false
     return (iId, oId)

-- | a 'Widget' which listens for incoming 'JWidget's which output
-- Strings. It adds these JWidgets to a list, and resends theirs
-- outputs to its output.
widgetList :: (XMLGenerator m, EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m), StringType m ~ TL.Text) => Widget m (In (JWidget (Out String)), Out String)
widgetList =
  do iId <- In  <$> nextId
     oId <- Out <$> nextId
     html $ let fromStringLit = HSP.fromStringLit in <ol id=(getId iId)></ol>
     js $ do bindId iId $ \(event, jwidget :: Exp (JWidget (Out String))) ->
               do e <- inVar $ selectId iId
                  d <- <input type="submit" value="x" />
                  li <- <li><% (jwidget # deref "xml" :: Exp ElementNode) %><% d %></li>
                  jid <- inVar $ (jwidget # deref "id")
                  runExp $ e # append li
                  deleteCB <- function $ \(jEvent :: JObject JEvent) ->
                    do runExp $ (selectExpr li) # remove
                       return false
                  runExp $ selectExpr d # click deleteCB
--                  callProc (jwidget # deref "init") ()
                  -- connect the inner and out widgets
                  jResend (Out (Id Javascript "js" jid) :: Out String) oId
                  return false
     return (iId, oId)

-- | similar to 'widgetList' except that instead of merging the outputs, it outputs an array containing the last message sent by each element of the list.
widgetList3 :: forall m a. (Show a, JType a, XMLGenerator m, EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m), StringType m ~ TL.Text) => Widget m (In (JWidget (Out a)), Out (Array a))
widgetList3 =
  do iId      <- In  <$> nextId
     oId      <- Out <$> nextId
     html $ let fromStringLit = HSP.fromStringLit in <ol class="nounphraselist"  id=(getId iId)></ol>
     js $ do ol  <- inVar $ selectId iId
             ol # sortable
             toArrayFn <- function $ \() ->
                               do ids <- inVar $ (selectExpr ol) # toArray
                                  arr <- new Array ()
                                  foreach ids $ \liId ->
                                      do -- window # alert ((selectExpr (string "#" .+. liId) # jData "msg") :: Exp String)
                                         arr # push (selectExpr (string "#" .+. liId) # jData "msg")
                                  -- window # alert arr
                                  return arr
             sortUpdateCB <- function $ \(event :: JObject JEvent) ->
                                    do sendMsg oId (call toArrayFn ())
                                       return true
             runExp $ (selectExpr ol) # bind ("sortupdate", sortUpdateCB)

             -- hook up incoming widgets
             bindId iId $ \(event, jwidget :: Exp (JWidget (Out a))) ->
               do d   <- <input class="delete_x" type="submit" value="x" />
                  (liId, _) <- jNextId'
                  xml <- inVar $ (jwidget # deref "xml" :: Exp ElementNode)
                  li  <- <li id=(getId liId)><% (jwidget # deref "xml" :: Exp ElementNode) %><% d %></li>
                  jid <- inVar $ (jwidget # deref "id")
                  runExp $ ol # append li
                  ol # refresh
                  deleteCB <- function $ \(jEvent :: JObject JEvent) ->
                    do runExp $ (selectExpr li) # remove
                       sendMsg oId (call toArrayFn ())
                       return false
                  runExp $ selectExpr d # click deleteCB
                  -- update the 'li' data when the inner widget changes
                  (updateId, updateIdVal) <- jNextId'

                  bindId (In updateId) $ \(event, msg :: Exp a) ->
                      do runExp $ (selectExpr li) # jSetData ("msg", msg)
                         sendMsg oId (call toArrayFn ())
                         return false
                  jConnect (Out (Id Javascript "js" jid) :: Out a) (In updateId)
                  runExp $ (selectExpr xml) # change_
                  runExp $ selectExpr (string "#Javascript_" .+. jShow jid) # change_ -- FIXME: this only works if the widget produces an output when change is triggered
                  return false
     return (iId, oId)


-- | similar to 'widgetList', but designed to work with 'JToggle' stuff
widgetList2 :: forall a m. (XMLGenerator m, EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m), StringType m ~ TL.Text) => Widget m (In (JToggle (Rec Int a)), In (JWidget (In (JToggle (Rec Int a)), Out (Array Int))), Out (Array Int))
widgetList2 =
  do iId <- In  <$> nextId
     wId <- In  <$> nextId
     oId <- Out <$> nextId
     html $ let fromStringLit = HSP.fromStringLit in <ol id=(getId iId)></ol>
     js $ do bindId wId $ \(event, jwidget) -> --  :: Exp (JWidget (In (JToggle (Rec Int a))))) ->
               do e <- inVar $ selectId iId
                  d <- <input type="submit" class="delete" value="x" />
                  liElem <- <li class="clickable"><% d %><% (jwidget # deref "xml" :: Exp ElementNode) %></li>
                  jid <- inVar $ (jwidget # deref "id")
                  jOidVal <- inVar $ (jwidget # deref "oid")
                  let jOid = Out (Id Javascript "js" jOidVal) :: Out (Array Int)
                  jIidVal <- inVar $ (jwidget # deref "iid")
                  let jIid = Out (Id Javascript "js" jIidVal) :: Out Int
                  runExp $ e # append liElem
                  li <- inVar $ selectExpr liElem
                  clickCB <- function $ \(jEvent :: JObject JEvent) ->
                    do runExp $ (selectExprContext (string "li", e)) # jRemoveClass "current"
                       runExp $ li # jAddClass "current"
                       runExp $ selectExpr (document) # unbind (id2event iId)
                       jReRecv iId (In (Id Javascript "js" jid) :: In (JToggle (Rec Int a)))
                       sendMsg (jIid :: Out Int) (int 99)
                       return false
                  runExp $ li # click clickCB
                  deleteCB <- function $ \(jEvent :: JObject JEvent) ->
                    do (li # jHasClass "current") ?
                         ( do a <- new Array ()
                              sendMsg oId a
                              runExp $ (li # jPrev ()) # click_
                         , return ()
                         )
                       runExp $ li # remove
                       return false
                  runExp $ selectExpr d # click deleteCB
--                  callProc (jwidget # deref "init") ()
                  -- connect the inner and out widgets
                  runExp $ (selectExprContext (string "li", e)) # jRemoveClass "current"
                  runExp $ li # jAddClass "current"
                  runExp $ selectExpr (document) # unbind (id2event iId)
                  jReRecv iId (In (Id Javascript "js" jid) :: In (JToggle (Rec Int a)))
--                  jConnect (Out iId) (In (Id Javascript jid) :: In (JToggle (Rec Int a)))
                  jResend jOid oId
                  -- clear existing selections
                  a <- new Array ()
                  sendMsg oId a

--                  jConnect (Out (Id Javascript jid) :: Out String) (In oId)
                  return false
{-
             bindId iId $ \(event, jtoggle :: Exp (JToggle (Rec Int a))) ->
               do
                  return false
-}
     return (iId, wId, oId)

-- | a 'Widget' which listens for incoming 'Strings' and outputs JWidgets which contain the string and which send the send which clicked.
itemFactoryWidget :: (NextId (Widget m)) => Widget m (In String, Out (JWidget (Out String)))
itemFactoryWidget =
  do constructor $ \str ->
       do (jid, jidVal) <- jNextId' -- create id for new widget
          e <- <span class="clickable" id=jid ><% str %></span>
          clickCB <- function $ \(jEvent :: JObject JEvent) ->
                    do sendMsg (Out jid) str
                       return false
          runExp $ selectExpr(e) # click clickCB
          new JWidget (e, jidVal)
{-
constructor' :: (NextId (Widget m)) => Exp (String -> JWidget (Out String)) -> Widget m (In String, Out (JWidget (Out String)))
constructor' make =
  do iId <- In <$> nextId
     oId <- Out <$> nextId
     js $ do bindId iId $ \(event, str :: JString) ->
               do -- window # alert str
                  -- window # alert (make)
                  sendMsg oId (call make str)
                  return false
     return (iId, oId)
-}

-- | a function which takes a javascript function of one argument that
-- returns a JWidget, and turns it into a Widget which listens for the
-- argument and outputs JWidgets.
constructor' :: forall m a. (JType a, NextId (Widget m)) => Exp (a -> JWidget (Out a)) -> Widget m (In a, Out (JWidget (Out a)))
constructor' make =
  do iId <- In <$> nextId
     oId <- Out <$> nextId
     js $ do bindId iId $ \(event, msg :: Exp a) ->
               do -- window # alert str
                  -- window # alert (make)
                  sendMsg oId (call make msg)
                  return false
     return (iId, oId)

-- | similar to constructor' except the JWidget does not depend on the value of the input argument
constructor0 :: (NextId (Widget m)) => Exp (() -> JWidget a) -> Widget m (In String, Out (JWidget a))
constructor0 make =
  do iId <- In  <$> nextId
     oId <- Out <$> nextId
     js $ do bindId iId $ \(event, str :: JString) ->
               do sendMsg oId (call make ())
                  return false
     return (iId, oId)

-- | a wrapper around constructor'. Perhaps not a very useful one. Should at least be parameterized around 'a' instead of 'String'.
constructor :: (NextId (Widget m)) => (JString -> HJScript (Exp (JWidget (Out String)))) -> Widget m (In String, Out (JWidget (Out String)))
constructor make =
  do fn <- js $ function make
     constructor' fn

-- Another set of functions for a widget similar to paragraphWidget

-- A widget which receives a message and shows it in a paragraph tag
outputToggle :: forall a m. (JShow a, XMLGenerator m, EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m), StringType m ~ TL.Text) => Widget m (In (JToggle (Rec Int a)))
outputToggle =
  do i <- In <$> nextId
     html $ let fromStringLit = HSP.fromStringLit in <p id=(getId i) />
     js $ do callback <- function $ \(event :: Exp JEvent, jtoggle :: Exp (JToggle (Rec Int a))) ->
               do e <- inVar $ selectId i
                  jtoggle # toggled ?
                   ( do runExp $ e # jSetText (jShow (second (jtoggle # msg)))
                   , do runExp $ e # jSetText (string "")
                   )
                  return false
             e <- inVar $ selectId i
             words <- new (Array :: Array String) ()
             runExp $ e # jSetData ("words", words)
             runExp $ selectExpr(document) # bind (id2event i, callback)
     return i

-- A widget which receives a message and shows it in a paragraph tag
outputMultiToggle :: forall a m. (JShow a, XMLGenerator m, EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m), StringType m ~ TL.Text) => Widget m (In (JToggle (Rec Int a)))
outputMultiToggle =
  do i <- In <$> nextId
     html $ let fromStringLit = HSP.fromStringLit in <p class="subjects" id=(getId i) />
     js $ do bindId i $ \(event :: Exp JEvent, jtoggle :: Exp (JToggle (Rec Int a))) ->
               do e <- inVar $ selectId i
                  words <- inVar $ (e # jData("words") :: Exp OrderedAssoc)
                  jtoggle # toggled ?
                   ( do words # insert (first (jtoggle # msg), (jShow $ second (jtoggle # msg)))
                   , do runExp $ words # deleteKey (first (jtoggle # msg))
                   )
                  vs <- inVar $ words # sortedValues ()
                  runExp $ e # jSetText (vs # join (string " "))
                  return false
             -- initialize with empty word list
             e <- inVar $ selectId i
             words <- new (OrderedAssoc :: OrderedAssoc) ()
             runExp $ e # jSetData ("words", words)
     return i

-- jOutputMultiToggle :: forall a m. (JType a, JShow a, XMLGenerator m, EmbedAsAttr m (Attr String Id), NextId (Widget m)) => Widget m (In (JToggle (Rec Int a)))
jOutputMultiToggle :: forall a. (JShow a) => HJScript (Exp (() -> (JWidget (In (JToggle (Rec Int a)), Out (Array Int)))))
jOutputMultiToggle = function $ \() ->
  do (jid, jidVal) <- jNextId'
     (iId, iIdVal) <- jNextId'
     (oId, oIdVal) <- jNextId'
     p <- <span />
     e <- inVar $ selectExpr p
     bindId (In jid) $ \(event :: Exp JEvent, jtoggle :: Exp (JToggle (Rec Int a))) ->
               do words <- inVar $ (e # jData("words") :: Exp OrderedAssoc)
                  jtoggle # toggled ?
                   ( do words # insert (first (jtoggle # msg), (jShow $ second (jtoggle # msg)))
                   , do runExp $ words # deleteKey (first (jtoggle # msg))
                   )
                  vs <- inVar $ words # sortedValues ()
                  runExp $ e # jSetText (vs # join (string " "))
                  return false
     bindId (In iId) $ \(event :: Exp JEvent, _msg :: Exp ()) ->
       do ks <- inVar $ (e # jData ("words") :: Exp OrderedAssoc) # keys ()
          sendMsg (Out oId) ks
          return false
     -- initialize with empty word list
     words <- new (OrderedAssoc :: OrderedAssoc) ()
     runExp $ e # jSetData ("words", words)
--     return (In jid)
     w <- new JWidget (p, jidVal)
     w # derefVar "oid" .=. oIdVal
     w # derefVar "iid" .=. iIdVal
     return w


-- jOutputMultiToggle :: (JShow a) => Exp (JWidget (In (JToggle (Rec Int a))))
-- jOutputMultiToggle = JConst "jOutputMultiToggle"

-- A javascript class for maintaining an ordered associative data

data OrderedAssoc = OrderedAssoc deriving Show
instance IsClass OrderedAssoc
instance HasConstructor OrderedAssoc () ()

forTest :: XMLGenT HJScript' ()
forTest =
  do a <- new Array ()
     forIn a (\k -> runExp $ val (a # (propertyVar k)))

keys :: () -> JObject OrderedAssoc -> Exp (Array t2)
keys = callMethod "keys"

insert :: (Exp Int, JString) -> Exp OrderedAssoc -> HJScript ()
insert = callVoidMethod "insert"

deleteKey :: (Exp Int) -> Exp OrderedAssoc -> JBool
deleteKey = callMethod "deleteKey"

values :: JObject OrderedAssoc -> Var Object
values = derefVar "values"

sortedValues :: () -> JObject OrderedAssoc -> Exp (Array t)
sortedValues = callMethod "sortedValues"

-- TODO: we could cache the sorted results, and expire the cache after an insert
orderedAssoc :: HJScript ()
orderedAssoc =
  do procedureDecl "OrderedAssoc" $ \() ->
       do vs <- new Object ()
          oaThis # values .=. vs
          -- keys
          keysFn <- function $ \() ->
            do ks <- new Array ()
               forIn (val (oaThis # values)) $ \k ->
                 do ks # push k
               numericSort <- function $ \(x :: JInt, y :: JInt) -> return (x .-. y)
               ks # sortBy numericSort
               return ks
          oaThis # (derefVar "keys") .=. keysFn
          -- insert
          insertFn <- procedure $ \(k :: Exp Int, v :: Exp ()) -> -- ^ these types are fake
            do (val (oaThis # values)) # propertyVar (jShow k) .=. v
          oaThis # derefVar "insert" .=. insertFn
          -- delete
          deleteFn <- function $ \(k :: Exp Int) ->
            return $ delete (val (oaThis # values) # propertyVar (jShow k))
          oaThis # derefVar "deleteKey" .=. deleteFn
          -- sorted values
          sortedFn <- function $ \() ->
            do ks <- inVar $ oaThis # keys ()
               vs <- inVar $ val (oaThis # values)
               arr <- new Array ()
               for (int 0) ((arrLength ks) .-. int 1) $ \i ->
                 do arr # push (val (vs # (propertyVar (val (ks #! i) :: JInt))))
               return arr
          oaThis # derefVar "sortedValues" .=. sortedFn
          return ()
    where
      oaThis :: JObject OrderedAssoc
      oaThis = this

-- | some javascript widget init code. This must go in your <head> for Widgets to work.
initWidgets :: HJScript ()
initWidgets =
  do procedureDecl "JToggle" $ \(tog :: JBool, m :: Exp ()) ->
       do (this :: JObject (JToggle ())) # derefVar "toggled" .=. tog
          (this :: JObject (JToggle ())) # derefVar "msg"     .=. m
          return ()
     -- JWidget constructor
     procedureDecl (show JWidget) $ \(e :: Exp ElementNode, i :: Exp Int) ->
       do (this :: JObject (JWidget ())) # (derefVar "xml") .=. e
          (this :: JObject (JWidget ())) # (derefVar "id")  .=. i
          return ()
     procedureDecl "JNounPhraseFragment" $ \() -> do return ()
     functionDecl "jSubmit" jSubmit'
     functionDecl "jInput"  jInput'
--     functionDecl "jOutputMultiToggle" jOutputMultiToggle'
     orderedAssoc
     currId <- varWith (int 0)
     functionDecl "nextId" $ \() ->
       do postinc currId
          return (val currId)
     return ()


-- | helper function which turns javascript into a <script> tag that calls the javascript via $(document).ready().
onReadyXML :: (XMLGenerator m, StringType m ~ TL.Text) => [HJScript ()] -> GenXML m
onReadyXML onReadyJs =
  let fromStringLit = HSP.fromStringLit in
  <script type="text/javascript">
    $(document).ready(function () {
      <% TL.pack $ unlines $ map (show . snd . evalHJScript) onReadyJs %>
    });
  </script>

-- | helper function which turns javascript into a <script> tag that calls the javascript via $(document).ready().
--
-- calls 'onReadyXML' but has a pure return value.
onReadyXML' :: [HJScript ()] -> HSP.XML
onReadyXML' = runIdentity . (unHSPT :: HSPT HSP.XML Identity HSP.XML -> Identity HSP.XML) . unXMLGenT . onReadyXML

-- | Writes the output of a 'Widget' to a hidden input field. This is
-- useful if you want to use a Widget in an html form, since you need
-- a way to get the output of the widget into the form data.
sink :: forall a m. (JType a, XMLGenerator m, StringType m ~ TL.Text, EmbedAsAttr m (Attr TL.Text Id), NextId (Widget m)) =>
        String -> String -> Widget m (In a)
sink n v =
  do i <- In <$> nextId
     let fromStringLit = HSP.fromStringLit
     html $ <input type="hidden" name=(TL.pack n)  id=(getId i) value=(TL.pack v) />
     js $ do bindId i $ \(event, str :: Exp a) ->
               do e <- inVar $ selectId i
--                  window # alert (stringify str)
                  runExp $ e # jSetVal (stringify str)
                  return false
     return i


-- | We need a way to specify how a Haskell value is represented as a
-- javascript value, and vice-versa. For example and [Int] in Haskell
-- might be an Array JInt in javascript.
class JSONCast a where
    type AsHaskell a
--    type AsJSON    a


instance JSONCast (Array a) where
    type AsHaskell (Array a) = [AsHaskell a]


-- | turn a 'Widget' into formlet.
widgetToForm ::
    ( XMLGenerator m
    , StringType m ~ TL.Text
    -- , Functor v
    , Monad v
    , FormInput i f
    , EmbedAsAttr m (Attr TL.Text Id)
    -- , EmbedAsAttr m (Attr TL.Text TL.Text)
    , NextId (Widget m)
    -- , Functor m
    -- , Monad m
    , JSON (AsHaskell a)
--    , JShow a
    , Show a
    , JType a
    -- , JSONCast a
    ) => String -> Widget m (In String, Out a) -> Form v i  [String] ([HJScript ()],[GenXML m]) (AsHaskell a)
widgetToForm prefix widget =
    (inputString (\n mv -> snd $ runWidget (prefix ++ show n) 0 $
                           do -- js $ window # alert (string "init widget")
                              let v = fromMaybe "" mv
                              (initId, oId) <- widget
                              iId <- sink (show n) v
                              connect oId iId
                              -- js $ window # alert (string v)
                              js $ sendMsg (inToOut initId) (string v)
                              return ()
              )
              Nothing) `transform` (transformEither $ \str -> case decode str of
                                          Ok a -> Right a
                                          (Error eStr) -> Left [eStr ++": " ++ str])

{-
widgetToForm ::
    ( XMLGenerator m
    , EmbedAsAttr m (Attr String Id)
    , NextId (Widget m)
    , Monad m
    , Monad v
    , JSON a
    , JShow a
    , Show a
    , JType a) => Widget m (Out a) -> Form ([HJScript ()],[GenXML m]) v a
widgetToForm widget =
    (F.input' (\n _ -> snd $ runWidget $ do oId <- widget
                                            iId <- sink n
                                            connect oId iId)
              Nothing) `check` (\str -> case decode str of
                                          Ok a -> Success a
                                          (Error eStr) -> Failure [eStr ++": " ++ str])
-}

-- sort :: (IsDeref d, Args d t) => d -> t -> HJScript ()

{-
function :: (FormalParams a t, VarsToExps a e) => (e -> HJScript (Exp r)) -> HJScript (Exp (t -> r))

JFunction    :: FormalParams a t => Maybe String -> a -> Block r -> Exp (t -> r)

-- | JFormal params represents parameters passed to a function along with their
-- corresponding types.
class (Show a, ParamType t) => FormalParams a t | a -> t where
  mkFParams :: (a -> b) -> Int -> a
  showsFParams :: a -> ShowS

class VarsToExps v e | v -> e, e -> v where
  v2e :: v -> e

class Show e => Args e t | e -> t where
  showsArgs :: e -> ShowS



-}

{-
addEvent :: Exp (a -> Bool) -> Widget m (Out a)
addEvent handler =
  do i <- nextId
     js $ runExp $ selectExpr(document) # bind ("sendMsg_" ++ show (unId i), handler)
     return (Out i)
-}

{-
     js $ do msgFn <- function $ \() ->
               do return false
             clickFn <- function $ \(jEvent :: JObject JEvent) ->
               do args <- new Array ()
                  runExp $ selectExpr(jThis) # triggerHandler ("sendMsg", (args :: Exp (Array Trigger)))
                  return false
             e <- inVar (selectId i)
             runExp $ e # jSetData ("msg", msgFn)
             runExp $ e # click clickFn
-}


{-
     js $ do fn <- objectVal
             bindSendText (i, fn)
             fn <- function $ \jObj ->
               do jObj # trigger "sendText"
             bindRecvTrigger (i, fn)
-}
{-

{-
Should toggleWord have Out Toggle? or just Out String?

If we have only, Out String, then how does:

 copyText outToggleWord inSubjectList

know when to perform the copy?

well, copyText seems a bit bogus anyway. outToggleWord should send the
Out String message whenever it wants by triggering the event. And,
anyone who is listening should receive it.

  do tw <- toggleWord "foo" :: Widget (Out String)
     d  <- display          :: Widget (In String)
     i  <- textInput        :: Widget (In Trigger, Out String)
     s  <- submit           :: Widget (Out Trigger)
     connect tw d           :: Widget ()
     connect i  d           :: Widget ()
-}

toggleWord :: (XMLGenerator m, EmbedAsAttr m (Attr String Id)) => String -> Widget m (Out Toggle, Out String)
toggleWord word =
  do i <- nextId
     let out = (Out i)
     html $ <span class="clickable" id=i><% word %></span>
     js $ do fn <- function $ \(jEvent :: JObject JEvent) ->
               do e <- inVar $ selectExpr jThis
                  (e # jData (string "toggled")) ?
                    ( do runExp $  e # jSetCss  ("background", "white")
                         runExp $ (e # jSetData ("toggled"   , false))
                    , do runExp $  e # jSetCss  ("background", "yellow")
                         runExp $ (e # jSetData ("toggled"   , true))
                    )
                  return false
             runExp $ (selectId i) # click fn
     js $ do fn <- function $ \o ->
               do return (o # jText)
             bindSendText (i, fn)
     return (out, out)

submit :: (XMLGenerator m, EmbedAsAttr m (Attr String Id)) => Widget m (Out Click)
submit =
  do i <- nextId
     html $ <input type="submit" id=i />
     return (Out i)

onClick :: Out Click -> HJScript (Exp (JEvent -> Bool)) -> Widget m ()
onClick cWidget action =
  do let (Id cId) = getId cWidget
     js $ do fn <- action
             runExp $ selectExpr (string $ ("#"  ++ show cId)) # click fn
     return ()

onToggle :: Out Toggle -> (HJScript (Exp (JEvent -> Bool)), HJScript (Exp (JEvent -> Bool))) -> Widget m ()
onToggle tOut (onTrue, onFalse) =
  do let i = getId tOut
     js $ do fn <- function $ \(jEvent :: JObject JEvent) ->
               do e <- inVar (selectExpr jThis)
                  r <- var :: HJScript (Var Bool)
                  (e # jData (string "toggled")) ?
                    ( do f <- onTrue
                         runExp (call f jEvent)
                    , do f <- onFalse
                         runExp (call f jEvent))
                  return (val r)
             runExp $ (selectId i) # click fn

copyText :: Out String -> In String -> HJScript (Exp (JEvent -> Bool))
copyText oWidget iWidget =
  do let iId = show . unId . getId $ iWidget
         oId = show . unId . getId $ oWidget
     function $ \(_jEvent :: JObject JEvent) ->
       do arr <- new Array ()
          arr # push (selectExpr (string $ "#" ++ iId))
          runExp $ selectExpr (string $ "#" ++ oId) # trigger ("sendText", arr)
          return false
-}

{-
  do sendMsg <- function $ \(event :: Exp JEvent, targetId :: JInt, msg :: Exp ()) ->
       do window # alert msg
          return false
     runExp $ selectExpr (document) # bind ("sendMsg", sendMsg)
-}
{-
  do functionDecl "bindRecvText" $ \jObj ->
       do callback <- function $ \(event :: JObject JEvent, txt :: JString) ->
            do -- window # alert (string "recvMsg callback")
               runExp $ selectExpr (this :: JObject JQuery) # jSetText (txt)
               return false
          return $ jObj # bind ("recvMsg", callback)
-}
{-
     functionDecl "bindSendText" $ \(jObj, extractFn) ->
       do callback <- function $ \(event :: JObject JEvent, destObj :: JObject JQuery) ->
            do arr <- new Array () :: HJScript (Exp (Array String))
               arr # push (call extractFn (selectExpr (jThis)))
               return $ destObj # trigger ("recvText", arr )
          return $ jObj # bind ("sendText", callback )

     functionDecl "bindRecvTrigger" $ \(jObj :: JObject JQuery, triggerAction :: Exp (JObject -> Bool)) ->
       do callback <- function $ \(event :: JObject JEvent) ->
            do return $ call triggerAction jThis
          return $ jObj # bind ("recvTrigger", callback)
-}

{-
     functionDecl "bindRecvToggle" $ \(jObj :: JObject JQuery, toggleAction :: Exp (Bool -> Bool)) ->
       do callback <- function $ \(event :: JObject JEvent, state :: JBool) ->
            do return $ call toggleAction state
          return $ jObj # bind ("recvToggle", callback)

     functionDecl "bindSendToggle" $ \(jObj :: JObject JQuery, getToggleState :: Exp (JQuery -> Bool)) ->
       do callback <- function $ \(event :: JObject JEvent, destObj :: JObject JQuery) ->
            do arr <- new Array () :: HJScript (Exp (Array Bool))
               arr # push (call getToggleState (selectExpr (jThis)))
               return $ destObj # trigger ("recvTrigger", arr )
          return $ jObj # bind ("sendToggle", callback)
-}

-- let (_,(js,_)) = (runWidget $ (toggleWord "foo" :: Widget Identity (Out Click))) in js

{-

What does it mean to 'send a message' in an object oriented language like javascript?

The 'message' could be an 'object'. But that does not really tell us anything about how the message gets passed.

We want the ability to have one-to-many and many-to-one message sending.

When on object receives a message, it will want to respond to it and
do something. The receiving objects could *poll* for messages, but
only if there is some for them to poll from. And polling seems like
trouble. Same with select.

another way to receive an message would be if the receiving object had a method:

 receiveMessage (data)

anyone who wants to send a message to that object, just calls the
function and passes the data. javascript is single threaded, so we do
not have to worry about any race conditions, etc.

So, now it is the duty of the sender to call the receive function on
the object that should receive the message. This also means that the
receiving object can not be garbage collected because the sender will
have a reference to it.

http://www.michaelhamrah.com/blog/2008/12/event-pooling-with-jquery-using-bind-and-trigger-managing-complex-javascript/

-}
{-

What if submit has two outputs? How do I specify which output to bind to?

right now we are just binding to sendMsg on the DOM element which has
a specific id. So submit would have to have two DOM elements so that
it could have two different ids.

Also, it means a widget must have a DOM element - even if it does not
really need one. For example, a timer might normally be pure
javascript.

The primary reason that the widget must be a DOM object is because we
use selectExpr to find the object associated with an id. This is
useful because the id acts as a pointer/handle that we can pass around
easily. But it is trouble if we want multiple inputs..

another option would be to register the callbacks with a central handler, and to use a different unique id system to generate ids for the callbacks.

we can still use bind / triggerHandle. But we use it on some global element like $(document).bind()

-}
{-

bindSendText :: (Id, Exp (JQuery -> String)) -> HJScript ()
bindSendText (Id i, fn) = callProc (JConst "bindSendText") (selectExpr (string $ "#" ++ show i), fn)

bindRecvText :: Id -> HJScript ()
bindRecvText (Id i) = callProc (JConst "bindRecvText") (selectExpr (string $ "#" ++ show i))

bindRecvTrigger :: (Id, Exp (JQuery -> Bool)) -> HJScript ()
bindRecvTrigger (i, fn) = callProc (JConst "bindRecvTrigger") ((selectId i), fn)

connect :: forall m a. (JType a, Show a) => (Out a) -> (In a) -> Widget m ()
connect (Out oWidget) (In iWidget) =
  js $ do sendMsg <- function $ \(event :: JObject JEvent, param :: Exp String) ->
            do outObj <- inVar (selectId oWidget)
               inObj  <- inVar (selectId iWidget)
               args <- new Array () :: HJScript (Exp (Array String))
               args # push param
               runExp $ inObj # triggerHandler ("recvMsg", args)
               return false
          outObj <- inVar (selectId oWidget)
          runExp $ outObj # bind ("sendMsg", sendMsg)
          return ()

  -}


{-
input :: (XMLGenerator m, EmbedAsAttr m (Attr String Id)) => String -> Widget m (In Trigger, Out String)
input v =
  do i <- nextId
     html $ <input type="text" id=i value=v />
     js $ do fn <- function $ \() ->
               do -- window # alert (string $ "msg callback")
                  return (selectId i # callMethod "val" ())
             runExp $ (selectId i) # jSetData ("msg", fn :: Exp (() -> String))
             -- window # alert (string $ "added msg to input")
             callback <- function $ \(event :: JObject JEvent, destObj :: JObject JQuery) ->
               do args <- new Array () :: HJScript (Exp (Array ()))
                  return $ (selectId i) # triggerHandler ("sendMsg", args )
             runExp $ (selectId i) # bind ("recvMsg", callback )
     return (In i, Out i)

output :: (XMLGenerator m, EmbedAsAttr m (Attr String Id)) => Widget m (In String)
output =
  do i <- nextId
     html $ <span id=i />
     js $ bindRecvText i
     return (In i)


submit :: (XMLGenerator m, EmbedAsAttr m (Attr String Id)) => Widget m (Out Trigger)
submit =
  do i <- nextId
     html $ <input type="submit" id=i />
     js $ do msgFn <- function $ \() ->
               do -- window # alert (string "msgFn")
                  return false
             clickFn <- function $ \(jEvent :: JObject JEvent) ->
               do args <- new Array ()
                  runExp $ selectExpr(jThis) # triggerHandler ("sendMsg", (args :: Exp (Array Trigger)))
                  return false
             e <- inVar (selectId i)
             runExp $ e # jSetData ("msg", msgFn)
             runExp $ e # click clickFn
     return (Out i)
  -}
{-
undef =
  do f <- var :: HJScript (Var a)
     callProc (val f) ()

mixedThis =
  do (this :: JArray Int)   # push (int 1)
     (this :: JArray String)   # push (string "foo")
     (this :: Exp TextNode) # appendData (string "bar")

heteroArray =
  do arr <- new Array ()
     arr # push' (int 1)
     arr # push' (string "foo")


data Assoc k v = Assoc k v deriving Show
instance (JType k, JType v, Show k, Show v) => IsClass (Assoc k v)
instance (JType k, JType v, Show k, Show v) => HasConstructor (Assoc k v) () ()

-- assocConstructor :: forall k v. (JType k, JType v, JShow k, JShow v, Show k, Show v) => k -> v -> HJScript ()
assocConstructor :: HJScript ()
assocConstructor = procedureDecl "Assoc" $ \() ->
  do addFn <- procedure $ \(k :: Exp String, v :: Exp Int) -> (aThis # propertyVar k) .=. v
     (aThis # derefVar "add") .=. addFn
     return ()
    where
      aThis :: JObject (Assoc Int Int)
      aThis = this

add :: (Show k, Show v, JType k, JType v) => (Exp k, Exp v) -> JObject (Assoc k v) -> HJScript ()
add = callVoidMethod "add"

lookupA :: (Show k, Show v, JType k, JType v) => Exp k -> JObject (Assoc k v) -> Exp v
lookupA = callMethod "lookup"

mapAssoc :: (Exp v -> Exp w) -> JObject (Assoc k v) -> JObject (Assoc k w)
-}

{-
instance Eq Id where
  (Id (JInt i)) == (Id (JInt j)) = i == j

instance Ord Id where
  compare (Id (JInt i)) (Id (JInt j)) = compare i j

instance Enum Id where
  succ (Id (JInt i)) = Id (JInt (succ i))
  pred (Id (JInt i)) = Id (JInt (pred i))
  toEnum i = Id (JInt i)
  fromEnum (Id (JInt i)) = i
  enumFrom (Id (JInt i)) = map (Id . JInt) (enumFrom i)
-}
{-
data Click
data Trigger = Trigger deriving Show
data Toggle a = Toggle deriving Show
data Set
data Clear

instance JType Trigger
instance (JType a)=> JType (Toggle a)
-}
{-
outgoing :: (JType a, JType b) => Id -> ((Exp JEvent, Exp a) -> HJScript (Exp b)) -> HJScript ()
outgoing oId callback =
  do outgoingCB <- function callback
     args <- new Array ()
     args # push (call outgoingCB ())
     runExp $ selectExpr (document) # triggerHandler (id2event oId, args)
     return ()
-}
{-
widgetList :: (XMLGenerator m, EmbedAsAttr m (Attr String Id)) => Widget m (In String, Out String)
widgetList =
  do wId <- nextId
     oId <- nextId
     html $ <ol id=wId></ol>
     js $ do incoming wId $ \(event, str :: JString) ->
               do e <- inVar $ selectId wId
                  jid <- inVar jNextId
                  li <- <li id=jid><% str %></li>
n                  clickCB <- function $ \(jEvent :: JObject JEvent) ->
                    do window # alert str
                       sendMsg (Out (Id jid)) str
                       return false
                  runExp $ selectExpr(li) # click clickCB
                  runExp $ e # append li
                  -- connect the inner and out widgets
                  jConnect (Out (Id jid) :: Out String) (In oId)
                  return false
     return (In wId, Out oId)
-}
{-
itemWidget :: Widget m (Out (Widget m ()))
itemWidget
-}
{-
jSendMsg :: (JType a, Show a) => Exp Int -> (Exp a) -> HJScript ()
jSendMsg iId msg =
  do args <- new Array ()
     args # push msg
     runExp $ selectExpr(document) # triggerHandler ((string "sendMsg_") .+. (jShow iId) , args)
  -}

{-
objectVal :: HJScript (Exp (JQuery -> String))
objectVal = function fn
  where
    fn :: JObject JQuery -> HJScript (Exp String)
    fn o = return (callMethod "val" () o)
-}

{-
toJS :: (NextId (Widget m)) => Widget HJScript' (Out String) -> Widget m (In String, Out (JWidget (Out String)))
toJS w =
  do iId <- nextId
     oId <- Out <$> nextId
     mapWidget (f iId oId) w
    where
      f iId oId (Out ident, i, (js, xml)) = ((In iId, oId), i, ([xmlToJs iId oId ident js xml] , []))
      xmlToJs iId oId (Id src i) js [xml] =
        do x <- xml
           jw <- new JWidget (x, i)
--           init <- procedure $ \() -> sequence_ js
--           (jw # derefVar "init") .=. init
           bindId iId $ \(event, str :: JString) ->
               do sendMsg oId jw
                  return false
-}
{-
constructor'' :: Exp (JWidget (Out String)) -> Widget m (In String, Out (JWidget (Out String)))
constructor'' make =
  do iId <- nextId
     oId <- Out <$> nextId
     js $ do bindId iId $ \(event, str :: JString) ->
               do sendMsg oId make
                  return false
     return (In iId, oId)
-}
