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
    --rec <- createSVGRectangle "pick" "10" "10" >>= setAttr "class" "pick" >>= appendTo svg
    --startAnimation $ hobble (x,y) 10.0 1.0 rec

spawnIntraGalacticCivilizations :: Svg -> Event -> Fay ()
spawnIntraGalacticCivilizations svg _ = do
    let r = 500
        x = 200
        y = 100
    --createSVGCircle "galaxy" (fromShow (r+50)) >>= moveTo (x+r, y+r) >>= appendTo svg
    let sector = (r, (x,y))
        galaxy = sector
    createGalacticSector galaxy sector svg

type Sector = (Double, Pos)
type Svg = JQuery

createGalacticSector :: Sector -> Sector -> Svg -> Fay ()
createGalacticSector g sec@(r, (x,y)) svg = do
    when (insideGalaxy sec g) $ do
        svgsector <- createSVGRectangle "sector" (fromShow (r*2)) (fromShow (r*2)) >>= moveTo (x, y) >>= appendTo svg
        return svgsector >>= click (\e -> do
            remove svgsector
            createGalacticSectors g ((r/2), (x,y)) svg)

    return ()

createGalacticSectors :: Sector -> Sector -> Svg -> Fay ()
createGalacticSectors g (r, (x,y)) svg = do
    createGalacticSector g (r, (x,y)) svg
    createGalacticSector g (r, (x+d,y)) svg
    createGalacticSector g (r, (x+d,y+d)) svg
    createGalacticSector g (r, (x,y+d)) svg
  where d = 2*r

insideGalaxy :: Sector -> Sector -> Bool
insideGalaxy (sr, (sx, sy)) sec = inside scx scy sec
                               || inside sx sy sec
                               || inside (sx+sd) sy sec
                               || inside (sx+sd) (sy+sd) sec
                               || inside (sx) (sy+sd) sec
    where scx = sx + sr
          scy = sy + sr
          sd = sr * 2

inside x y (r, (gx, gy)) = dist2 (x-cx) (y-cy) < (r*r)
    where cx = gx+r
          cy = gy+r

dist2 dx dy = (dx * dx) + (dy * dy)

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
