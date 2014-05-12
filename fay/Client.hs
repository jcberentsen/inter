{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module Client where

import Language.Fay.Yesod hiding (fromString)
import JQuery
import Fay.Text as T
import FayRef
import SharedTypes
import FFI
import Prelude
import Three
import Webgl
import Animation
import Svg

main :: Fay ()
main = ready $ do
    body <- select "body"
    svg <- select "#svg"
    mouseRef <- newFayRef (0::Double, 0::Double)
    createSVGGroup >>= setAttr "id" "#svggroup" >>= appendTo svg
    select "#spawn" >>= click (spawnIntraGalacticCivilizations svg)
    getDocument >>= select >>= click (onCanvasClick svg)

    select "#glcanvas" >>= do3dStuff mouseRef

    log <- select "#log"
    event_source <- newEventSource "/event"
    addEventListener "log" (handleLogEvent log) event_source

do3dStuff :: FayRef (Double, Double) -> JQuery -> Fay ()
do3dStuff mouseRef glcanvas = do
    (scene, camera, renderer) <- init3d glcanvas
    getWindow >>= select >>= resize (rendererOnResize camera renderer)

    getDocument >>= select >>= mousemove (onCanvasMouseMove mouseRef)

    loadColladaMesh scene
    animate3d scene camera renderer mouseRef

fromRational :: a -> Double
fromRational = undefined

onCanvasClick :: JQuery -> Event ->  Fay ()
onCanvasClick svg e = do
    x <- eventClientX e
    y <- eventClientY e
    logF (x,y)
    rec <- createSVGRectangle "pick" "10" "10" >>= setAttr "class" "pick" >>= appendTo svg
    startAnimation $ hobble (x,y) 10.0 1.0 rec

spawnIntraGalacticCivilizations :: JQuery -> Event -> Fay ()
spawnIntraGalacticCivilizations svg _ = do
    createGalacticSector 500 (200,100) svg

createGalacticSectors r (x,y) svg = do
    createGalacticSector r (x,y) svg
    createGalacticSector r (x+d,y) svg
    createGalacticSector r (x+d,y+d) svg
    createGalacticSector r (x,y+d) svg
  where d = 2*r

createGalacticSector r (x,y) svg = do
    sector <- createSVGRectangle "sector" (fromShow (r*2)) (fromShow (r*2)) >>= moveTo (x, y) >>= appendTo svg
    return sector >>= click (const $ createGalacticSectors (r/2) (x,y) svg)

    --createSVGCircle "galaxy" (fromShow r) >>= moveTo (x+r, y+r) >>= appendTo svg
    return ()

renderServerFeedback :: JQuery -> T.Text -> Fay ()
renderServerFeedback node t = setText t node >> return ()

makeMultipleSVGRects :: JQuery -> Fay [((Int,Int), JQuery)]
makeMultipleSVGRects svg = do
    let positions = [ (i,j) | i <- [1..8::Int], j <- [1..8::Int] ]
    rects <- forM positions (\(i,j) -> createSVGRectangle "stress" "100" "100" >>= setAttr "class" "stress" >>= appendTo svg)
    return $ zip positions rects

getWindow :: Fay Element
getWindow = ffi "window"

getDocument :: Fay Element
getDocument = ffi "document"

newEventSource :: T.Text -> Fay Element
newEventSource = ffi "new EventSource(%1)"

addEventListener :: T.Text -> (Event -> Fay f) -> Element -> Fay ()
addEventListener = ffi "%3['addEventListener'](%1,%2)"

handleLogEvent :: JQuery -> Event -> Fay ()
handleLogEvent node e = do
    eData <- eventData e
    setText eData node >> return ()

eventData :: Event -> Fay T.Text
eventData = ffi "%1['data']"
