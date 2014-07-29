module Game where

import Prelude
import Data.Text
import Data.Monoid (mappend)
import SharedTypes

data Game = Game
    { gameName :: Text
    , gameDestination :: WorldPos
    } deriving (Show)

initialGame :: Game
initialGame = Game "" (0,0)

gameStart :: Text -> Game -> IO Game
gameStart name game = do
    let game' = game { gameName = name}
    (putStrLn . show . gameName) game'
    return game'

gameUserClicked :: WorldPos -> Game -> IO (Game, ClientEvent)
gameUserClicked pos game = do
    return (game { gameDestination = pos }, NewDestination pos)
