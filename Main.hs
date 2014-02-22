{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens
import Control.Monad.State (State, execState, get, put)
import Data.List (deleteBy)

type Direction = (Int, Int)

type GridPos = (Int, Int)
type Pod = GridPos

data World = World { _pod :: Pod
                   , _vel :: Direction
                   }

makeLenses ''World

update :: Float -> World -> World
update time = execState $ do
    updateWorld time

step :: Direction -> GridPos -> GridPos
step (dx, dy) (x, y) = (x+dx, y+dy)

updateWorld :: Float -> State World ()
updateWorld time = do
    world <- get
    put $ move world

move world@(World _ dir) = case dir of
    (0,0) -> world
    _ -> pod %~ (step dir) $ world

radi :: Float
radi = 5

initialWorld :: World
initialWorld = World (1,0) (0,0)

windowed :: Display
windowed = InWindow "Inter" (700, 500) (10, 10)

full :: Display
full = FullScreen (1024, 768)

toScreen :: GridPos -> Picture -> Picture
toScreen (tx,ty) p = Translate (2*radi*(fromIntegral tx)) (2*radi*(fromIntegral ty)) p

pic :: World -> Picture
pic world = Pictures
    [ Color green $ Pictures $ [toScreen (world^.pod) (circleSolid radi)]
    , Color white $ Translate (-200) (200) $ Scale 0.4 0.4 $ text "Score: 9999983"
    ]

handleEvent :: Event -> World -> World
handleEvent e world = case e of
    (EventKey (SpecialKey KeyLeft) Down _ _) -> vel.~(-1,0) $ world
    (EventKey (SpecialKey KeyRight) Down _ _) -> vel.~(1,0) $ world
    (EventKey (SpecialKey KeyUp) Down _ _) -> vel.~(0,1) $ world
    (EventKey (SpecialKey KeyDown) Down _ _) -> vel.~(0,-1) $ world
    (EventKey (SpecialKey KeySpace) Down _ _) -> vel.~(0,0) $ world
    _ -> world

advanceTime :: Float -> World -> World
advanceTime dt world = update dt world

fps :: Int
fps = 20

main :: IO ()
main = play windowed black fps initialWorld pic handleEvent advanceTime


{- TODO
 -}
