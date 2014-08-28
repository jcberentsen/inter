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
