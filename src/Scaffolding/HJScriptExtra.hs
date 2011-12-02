{- | A bunch of stuff that should be cleaned up and moved into upstream HJScript #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Scaffolding.HJScriptExtra where

import HJScript hiding (round)
import HJScript.DOM (ElementNode, IsElementNode, window, alert)
import HJScript.Objects.JQuery (JQuery)
import Text.JSON (JSValue(..), fromJSObject, encode, fromJSString)


-- this is really the same as the one in HJScript.Objects.JQuery, but with a clearer type signature
-- * JQuery Things
     
data JEvent = JEvent deriving (Eq, Ord, Read, Show)
instance IsClass JEvent

jQuery :: JObject JQuery
jQuery = JConst "jQuery"

selectExprContext :: (Exp e, Exp c) -> JObject JQuery
selectExprContext (e, c) = methodCall "jQuery" (e,c) window

jSetCss :: (IsJString name, IsJString val) => (name, val) -> JObject JQuery -> JObject JQuery
jSetCss (n, v) =  callMethod "css" (toJString n, toJString v)

jAddClass :: (IsJString name) => name -> JObject JQuery -> JObject JQuery 
jAddClass n = callMethod "addClass" (toJString n)

jHasClass :: (IsJString name) => name -> JObject JQuery -> Exp Bool
jHasClass n = callMethod "hasClass" (toJString n)

jRemoveClass :: (IsJString name) => name -> JObject JQuery -> JObject JQuery 
jRemoveClass n = callMethod "removeClass" (toJString n)

jEach :: Exp ((Int, ElementNode) -> Bool) -> JObject JQuery -> JObject JQuery
jEach = callMethod "each"

jAttr :: (IsJString name) => name -> JObject JQuery -> JString
jAttr n = callMethod "attr" (toJString n)

jSetAttr :: (IsJString name, IsJString value) => name -> value -> JObject JQuery -> JString
jSetAttr n v = callMethod "attr" (toJString n, toJString v)

jSetVal :: (IsJString value) => value -> JObject JQuery -> JObject JQuery
jSetVal v = callMethod "val" (toJString v)

jHtml :: JObject JQuery -> JString
jHtml = callMethod "html" ()

jChildren :: () -> JObject JQuery -> JObject JQuery
jChildren = callMethod "children"

jFirst :: () -> JObject JQuery -> JObject JQuery
jFirst = callMethod "first"

jPrev :: () ->  JObject JQuery -> JObject JQuery
jPrev = callMethod "prev"

jNext :: () -> JObject JQuery -> JObject JQuery
jNext = callMethod "next"

jText :: () -> JObject JQuery -> JString
jText = callMethod "text"

jRemove :: () -> JObject JQuery -> JObject JQuery
jRemove = callMethod "remove"

class IsContent a
instance IsContent ElementNode
instance IsContent JQuery
instance IsContent String

jBefore :: (IsContent c) => (Exp c) -> JObject JQuery -> JObject JQuery
jBefore content = callMethod "before" content


bind :: (IsJString name, Args (JString, e) t) => (name, e) -> JObject JQuery -> JObject JQuery
bind (name, e) = callMethod "bind" (toJString name, e)

unbind :: (IsJString name) => name -> JObject JQuery -> JObject JQuery
unbind name = callMethod "unbind" (toJString name)

click :: Exp (JEvent -> Bool) -> JObject JQuery -> JObject JQuery
click = callMethod "click"

click_ :: JObject JQuery -> JObject JQuery
click_ = callMethod "click" ()

change :: Exp (JEvent -> Bool) -> JObject JQuery -> JObject JQuery
change = callMethod "change"

change_ :: JObject JQuery -> JObject JQuery
change_ = callMethod "change" ()

-- click_ :: JObject JQuery -> JObject JQuery
-- click_ = callMethod "click" ()

jStopPropogation :: () -> JObject JEvent -> HJScript ()
jStopPropogation = callVoidMethod "stopPropogation"

jPreventDefault :: () -> JObject JEvent -> HJScript ()
jPreventDefault = callVoidMethod "preventDefault"

remove :: JObject JQuery -> JObject JQuery
remove = callMethod "remove" ()

jData :: (IsJString name, JType a) => name -> JObject JQuery -> Exp a
jData name = callMethod "data" (toJString name)

jSetData :: (IsJString name, Args (JString, a) t) => (name , a) -> JObject JQuery -> JObject JQuery
jSetData (name, val) = callMethod "data" (toJString name, val)

-- | Methods on array
push' :: Exp a -> JArray t -> HJScript ()
push' arg = callVoidMethod "push"  arg

-- | join all elements of an array into a string
join :: JString -> JArray t -> JString
join = callMethod "join"

shift :: JArray t -> Exp t
shift = callMethod "shift" ()

getInnerHtml ::  IsElementNode n => Exp n -> Exp String
getInnerHtml = callMethod "innerHTML" ()

-- JQuery sortable

sortable :: JObject JQuery -> HJScript ()
sortable = callVoidMethod "sortable" ()

enable :: JObject JQuery -> HJScript ()
enable = callVoidMethod "sortable" (string "enable")

refresh :: JObject JQuery -> HJScript ()
refresh = callVoidMethod "sortable" (string "refresh")


toArray :: JObject JQuery -> Exp (Array String)
toArray = callMethod "sortable" (string "toArray")

-- JSON2

stringify :: (Args a t) => a -> Exp String
stringify = call (JConst "JSON.stringify")

parseJSON :: (Args str t, IsJString str) => str -> Exp a
parseJSON = call (JConst "JSON.parse")

-- click :: 

triggerHandler :: (IsJString name) => (name, Exp (Array t)) -> JObject JQuery -> JBool
triggerHandler (n, args) = callMethod "triggerHandler" (toJString n, args)

jThis :: JObject JQuery 
jThis = this

-- Array

sort :: () -> JArray t -> HJScript ()
sort = callVoidMethod "sort"

sortBy :: Exp ((a, a) -> Int) -> JArray t -> HJScript ()
sortBy = callVoidMethod "sort"

sortTest :: HJScript ()
sortTest =
  do a <- new Array ()
     a # push (int 30)
     a # push (int 4)
     a # push (int 40)
     numericSort <- function $ \(x :: JInt, y :: JInt) -> return (x .-. y)
     a # sort ()
     window # alert (a)
     a # sortBy  (numericSort)
     window # alert (a)

-- | turn a list of javascript expressions into a javascript array of expressions
listArray :: [Exp t] -> HJScript (JArray t)
listArray l = 
  do a <- new Array ()
     mapM_ (\e -> a # push e) l
     return a

-- * core

property :: (IsDeref d, JShow p) => Exp p -> d -> Exp a
property str obj = val $ JPropertyVar obj str


toJObject :: (IsClass a) => JSValue -> HJScript (JObject a)
toJObject v =
    case v of
      (JSObject jsobj) ->
          do let properties = fromJSObject jsobj
             obj <- new Object ()
             mapM_ (addProperty obj) properties
             return (JCastObject obj)
      _ -> error $ "jsvalueToExp: " ++ encode v ++ " is not an object."
    where
      addProperty :: (IsClass a) => JObject a -> (String, JSValue) -> HJScript ()
      addProperty obj (name, JSNull)                   = obj # derefVar name .=. (jnull :: Exp String) -- fake out type checker
      addProperty obj (name, JSBool b)                 = obj # derefVar name .=. (bool b)
      addProperty obj (name, (JSRational False i)) = obj # derefVar name .=. (int (round i))
      addProperty obj (name, (JSRational True i))  = obj # derefVar name .=. (float (fromRational i))
      addProperty obj (name, JSString str)             = obj # derefVar name .=. string (fromJSString str)
      addProperty obj (name, JSArray  vals)            = do arr <- new Array ()
                                                            mapM_ (addElem arr) vals
                                                            obj # derefVar name .=. arr
      addProperty obj (name, jsobj@(JSObject{}))       = do subObj <- toJObject jsobj
                                                            obj # derefVar name .=. (subObj :: JObject String) -- fake out type checker
      addElem :: JArray t -> JSValue -> HJScript ()
      addElem arr (JSNull)                 = arr # push' (jnull :: Exp String) -- fake out type checker
      addElem arr (JSBool b)               = arr # push' (bool b)
      addElem arr (JSRational False i) = arr # push' (int (round i))
      addElem arr (JSRational True i)  = arr # push' (float (fromRational i))
      addElem arr (JSString str)           = arr # push' (string (fromJSString str))
      addElem arr (JSArray  vals)          = do subArr <- new Array ()
                                                mapM_ (addElem subArr) vals
                                                arr # push' subArr
      addElem arr jsobj@(JSObject{})       = do subObj <- toJObject jsobj
                                                arr # push' (subObj :: JObject String) -- fake out type checker


      
