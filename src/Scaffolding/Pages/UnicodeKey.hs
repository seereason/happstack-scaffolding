-- |Pages which are not part of our core
{-# LANGUAGE FlexibleContexts, FlexibleInstances, PackageImports, RankNTypes, ScopedTypeVariables, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wwarn -F -pgmFtrhsx -fcontext-stack=40 -fno-warn-orphans -fno-warn-name-shadowing #-}
module Scaffolding.Pages.UnicodeKey
    ( unicodeKeyPage
    ) where

import Data.Generics.SYB.WithClass.Instances ()
import Data.List (genericLength, genericSplitAt, unfoldr)
import Happstack.Server (Happstack, Response, ToMessage)
import HJScript.Utils ()
import HSP
import qualified HSX.XMLGenerator as HSX
import Prelude hiding (null)
import Scaffolding.AppConf (HasAppConf)
import Scaffolding.Pages.AppTemplate (MonadRender, template)
import Scaffolding.ProfileData.User (MonadUser)
import qualified Scaffolding.Unicode.Render as UR (charsOfInterest)
import Web.Routes.RouteT (MonadRoute, URL)
import Web.Routes.XMLGenT ()

unicodeKeyPage :: (Happstack m, MonadRoute m, MonadUser m, MonadRender m, HasAppConf m, EmbedAsAttr m (Attr String (URL m)), ToMessage (HSX.XML m)) => m Response
unicodeKeyPage =
    template "Unicode Keys" ([] :: [XML]) text
    where
      text = table tuplesOfInterest'
      tuplesOfInterest' :: (XMLGenerator x) => [[ GenXML x ]]
      tuplesOfInterest' = map (map (spn . fmt)) tuplesOfInterest
          where spn t = <span> <% t %> </span>
                fmt (s,c) = <table> <tr> <td style="width: 4em"> <% show c %> </td> <td style="width: 1em"> <% (c:[]) %> </td> <td> <% s %> </td> </tr></table>
      tuplesOfInterest :: [[ (String, Char) ]]
      tuplesOfInterest = tablify n $ UR.charsOfInterest
          where n :: Integer
                n = 200
      table :: (XMLGenerator x) => [[GenXML x]] -> GenXML x
      table m =
          t
          where
            t = <table><% rows %></table>
            rows = map (\r -> <tr> <% r %> </tr>) columns
            columns = map (map (\c -> <td> <% c %> </td>)) m

tablify :: (Integral w, Show a) => w -> [a] -> [[a]]
tablify pageWidth xs =
    unfoldr (f ncols) xs
        where
          f n ys = case (genericSplitAt n ys) of
                     ([],[]) -> Nothing
                     x -> Just x
          ncols = max 1 (pageWidth `div` cellWidth)
          cellWidth = maximum $ map (genericLength . show) xs
