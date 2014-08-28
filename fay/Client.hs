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
    call (UserClicked world_pos) $ \(ShipUpdate ship) -> do
        --logF $! "world pos = " `T.append` (fromShow dest_world_pos)
        addWaypoints svg (shipPos ship) (shipWaypoints ship)
        --ship <- moveShip svg dest_world_pos
        return ()

drawShip :: JQuery -> WorldPos -> Fay ()
drawShip svg world_pos = do
    (sx, sy) <- worldToScreenPos svg world_pos
    createSVGRectangle "ship" "20" "20" >>= moveTo (sx, sy) >>= appendTo svg
    return ()

moveShip :: JQuery -> WorldPos -> Fay JQuery
moveShip svg world_pos = do
    (sx, sy) <- worldToScreenPos svg world_pos
    ship <- getShip svg
    return ship

getShip :: JQuery -> Fay JQuery
getShip svg = childrenMatching "#ship" svg >>= first

addWaypoints :: JQuery -> WorldPos -> [WorldPos] -> Fay JQuery
addWaypoints svg pos waypoints = do
    ship_pos <- worldToScreenPos svg pos
    waypositions <- mapM (worldToScreenPos svg) waypoints
    let waypos = Prelude.head waypositions
    waypoint <- createSVGRectangle "waypoint" "5" "5" >>= moveTo waypos >>= appendTo svg
    drawPath svg ship_pos waypositions

drawPath :: JQuery -> Pos -> [Pos] -> Fay JQuery
drawPath svg pos waypositions = do
    oldpath <- childrenMatching "#path" svg >>= first
    remove oldpath
    createSVGPath "path" waypositions >>= appendTo svg

spawnIntraGalacticCivilizations :: Svg -> Event -> Fay ()
spawnIntraGalacticCivilizations svg _ = do
    call (StartGame "Unsaved Inter game") $ \result -> do
        let r = 500
            x = 200
            y = 100
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
