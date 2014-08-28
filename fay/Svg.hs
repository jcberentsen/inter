{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module Svg where

import Language.Fay.Yesod hiding (fromString)
import JQuery
import Fay.Text as T
import FayRef
import SharedTypes
import FFI
import Prelude
import Webgl
import Animation

type Pos = (Double, Double)

llogF :: f -> Fay ()
llogF = ffi "console.log(%1)"

createElementNS :: T.Text -> T.Text -> Fay Element
createElementNS = ffi "document.createElementNS(%1,%2)"

createSVGElement tag = createElementNS "http://www.w3.org/2000/svg" tag >>= select

fromShow x = fromString (show x)

moveTo :: Pos -> JQuery -> Fay JQuery
moveTo (x,y) = setAttr "transform" ("translate(" `T.append` (fromShow x)
                     `T.append` "," `T.append` (fromShow y) `T.append` ")")

startAnimation :: (Int -> Fay ()) -> Fay ()
startAnimation animation = requestAnimationFrame (repeat 0) >> return ()
  where
    repeat n = do
        animation n
        requestAnimationFrame (repeat (n+1))

hobble :: Pos -> Double -> Double -> JQuery -> (Int -> Fay ())
hobble (x,y) width speed node n = do
    let nm = (sin (speed * (fromIntegral n))) * (width / 2)
    moveTo (x + nm, y) node
    return ()

createSVGRectangle :: T.Text -> T.Text -> T.Text -> Fay JQuery
createSVGRectangle ident w h = createSVGElement "rect" >>= setAttr "id" ident  >>= setAttr "width" w >>= setAttr "height" h

createSVGCircle :: T.Text -> T.Text -> Fay JQuery
createSVGCircle ident radi = createSVGElement "circle" >>= setAttr "id" ident  >>= setAttr "r" radi

createSVGPath :: T.Text -> [Pos] -> Fay JQuery
createSVGPath ident points = do
    let points_attr = T.concat (Prelude.map (\(x,y) -> (fromShow x) `T.append` "," `T.append` (fromShow y) `T.append` " ") points)
    let style = "fill:none;stroke:white;stroke-width:3"
    createSVGElement "polyline" >>= setAttr "id" ident  >>= setAttr "points" points_attr >>= setAttr "style" style

-- <polyline points="20,20 40,25 60,40 80,120 120,140 200,180"
--   style="fill:none;stroke:black;stroke-width:3" />

createSVGGroup :: Fay JQuery
createSVGGroup = createSVGElement "g"
