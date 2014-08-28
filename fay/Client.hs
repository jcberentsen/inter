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
    click (onClickNewDestination svg) svg

    select "#glcanvas" >>= do3dStuff mouseRef

    log <- select "#log"
    event_source <- newEventSource "/event"
    addEventListener "log" (handleLogEvent log) event_source
    drawShip svg (WorldPos 0 0)

do3dStuff :: FayRef (Double, Double) -> JQuery -> Fay ()
do3dStuff mouseRef glcanvas = do
    (scene, camera, renderer) <- init3d glcanvas
    getWindow >>= select >>= resize (rendererOnResize camera renderer)

    getDocument >>= select >>= mousemove (onCanvasMouseMove mouseRef)

    loadColladaMesh scene
    animate3d scene camera renderer mouseRef

fromRational :: a -> Double
fromRational = undefined

screenToWorldPos :: JQuery -> Pos -> Fay WorldPos
screenToWorldPos svg pos = do
    svg_w <- getWidth svg
    svg_h <- getHeight svg
    let sx = fst pos
    let sy = snd pos
    return $ WorldPos (sx - svg_w / 2) (sy - svg_h / 2)

worldToScreenPos :: JQuery -> WorldPos -> Fay Pos
worldToScreenPos svg world_pos = do
    svg_w <- getWidth svg
    svg_h <- getHeight svg
    let wx = world_x world_pos
    let wy = world_y world_pos
    let sx = (wx + svg_w / 2) * 1800 / svg_w
    let sy = (wy + svg_h / 2) * 1200 / svg_h
    return (sx, sy)

onClickNewDestination :: JQuery -> Event ->  Fay ()
onClickNewDestination svg e = do
    logF $! e
    x <- eventClientX e
    y <- eventClientY e
    world_pos <- screenToWorldPos svg (x, y)
    logF $! "pos= " `T.append` (fromShow world_pos)
    call (UserClicked world_pos) $ \(NewDestination dest_world_pos) -> do
        logF $! "world pos = " `T.append` (fromShow dest_world_pos)
        moveShip svg dest_world_pos
        --startAnimation $ hobble (wx, wy) 10.0 1.0 rec
        return ()

drawShip :: JQuery -> WorldPos -> Fay ()
drawShip svg world_pos = do
    (sx, sy) <- worldToScreenPos svg world_pos
    createSVGCircle "ship" "50" >>= moveTo (sx, sy) >>= appendTo svg
    return ()

moveShip :: JQuery -> WorldPos -> Fay ()
moveShip svg world_pos = do
    (sx, sy) <- worldToScreenPos svg world_pos
    ship <- childrenMatching "#ship" svg >>= first >>= moveTo (sx, sy)
    logF $! ship
    return ()

spawnIntraGalacticCivilizations :: Svg -> Event -> Fay ()
spawnIntraGalacticCivilizations svg _ = do
    call (StartGame "Unsaved Inter game") $ \result -> do
        let r = 500
            x = 200
            y = 100
        --createSVGCircle "galaxy" (fromShow (r+50)) >>= moveTo (x+r, y+r) >>= appendTo svg
        let sector = (r, (x,y))
            galaxy = sector
        createGalacticSector galaxy sector svg
        logF result

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
