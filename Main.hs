{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Control.Monad.State (State, execState, get, put)

type Direction = (Float, Float)

type Pos = (Float, Float)
type Pod = Pos

data World = World { _pod :: Pod
                   , _dir :: Direction
                   , _speed :: Float
                   , _mouse :: Maybe Pos
                   , _event :: Maybe Event
                   }

makeLenses ''World

update :: Float -> World -> World
update time = execState $ do
    updateWorld time

step :: Direction -> Float -> Pos -> Pos
step (dx, dy) speed (x, y) = (x + dx * speed, y + dy * speed)

updateWorld :: Float -> State World ()
updateWorld _ = do
    world <- get
    put $ move world

move :: World -> World
move world = case world ^. dir of
    (0,0) -> world
    _ -> pod %~ (step (world ^. dir) (world ^. speed) ) $ world

radi :: Float
radi = 5

initialWorld :: World
initialWorld = World (0,0) (0,0) 1.0 Nothing Nothing

windowed :: Display
windowed = InWindow "Inter" (700, 500) (10, 10)

full :: Display
full = FullScreen (1024, 768)

toScreen :: Pos -> Picture -> Picture
toScreen (tx,ty) p = Translate tx ty p

pic :: World -> Picture
pic world = Pictures
    [ Color green $ Pictures $ [toScreen (world^.pod) (circleSolid radi)]
    , Color white $ Translate (-400) (200) $ Scale 0.15 0.15 $ text ("Score: " ++ show (world^.mouse) ++ " " ++ (show (world^.pod)) ++ " " ++ (show (world^.event)))
    ]

handleEvent :: Event -> World -> World
handleEvent e world = case e of
    (EventKey (SpecialKey KeyUp) Down _ _) -> speed %~ (+1) $ world
    (EventKey (SpecialKey KeyDown) Down _ _) -> speed %~ (+(-1)) $ world
    (EventKey (SpecialKey KeySpace) Down _ _) -> dir.~(0,0) $ world
    (EventMotion mouse_pos) -> mouse .~ (Just mouse_pos) $ world
    (EventKey (MouseButton LeftButton) Down _ pos) -> let pod_pos = world ^.pod in dir .~ (direction pos pod_pos) $ world
    _ -> event .~ (Just e) $ world

direction :: Pos -> Pos -> Direction
direction dest origin = let (x,y) = diff dest origin
                            len = sqrt $ x*x + y*y
                    in
                    (x/len, y/len)

diff :: Pos -> Pos -> Direction
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

advanceTime :: Float -> World -> World
advanceTime dt world = update dt world

fps :: Int
fps = 20

main :: IO ()
main = play windowed black fps initialWorld pic handleEvent advanceTime


{- TODO
 -}
