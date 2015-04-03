module Game where

import Prelude
import Data.Text
import Data.Monoid (mappend)
import SharedTypes

data Game = Game
    { gameName :: Text
    , gameShip :: Ship
    } deriving (Show)

initialGame :: Game
initialGame = Game "" $ Ship (WorldPos 0 0) [WorldPos 0 0]

gameStart :: Text -> Game -> IO Game
gameStart name game = do
    let game' = game { gameName = name}
    (putStrLn . show . gameName) game'
    return game'

gameUserClicked :: WorldPos -> Game -> IO (Game, ClientEvent)
gameUserClicked pos game = do
    let ship = gameShip game
    let waypoints = pos : (shipWaypoints ship)
    let ship' = ship { shipWaypoints=waypoints }
    return (game { gameShip = ship'}, ShipUpdate ship')

gameUserDeleteLastWaypoint :: Game -> IO (Game, ClientEvent)
gameUserDeleteLastWaypoint game = do
    let ship = gameShip game
    let allWaypoints = shipWaypoints ship
    let waypoints = Prelude.drop 1 allWaypoints
    let ship' = ship { shipWaypoints=waypoints }
    return (game { gameShip = ship'}, ShipUpdate ship')

gameEmbark :: Game -> IO (Game, ClientEvent)
gameEmbark game = do
    let ship = gameShip game
    let allWaypoints = shipWaypoints ship
    let waypoints = Prelude.init allWaypoints
    let ship' = ship { shipWaypoints=waypoints }
    return (game { gameShip = ship'}, ShipUpdate ship')

gameScan :: Game -> IO (Game, ClientEvent)
gameScan game = do
    let ship = gameShip game
    let ship' = ship
    let scanResult = [ Body (WorldPos 100 100) 1E24, Body (WorldPos 0 0) 1E30 ]
    return (game { gameShip = ship'}, ScanResult scanResult)
