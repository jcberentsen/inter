{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Language.Fay.Yesod
import Data.Data
import Prelude (Read, Show, Int, Double)
#ifdef FAY
import Fay.Text as T
#else
import Data.Text as T
#endif

data Command =
    StartGame T.Text (Returns T.Text)
  | UserClicked WorldPos (Returns ClientEvent)
  | DeleteLastWaypoint (Returns ClientEvent)
  | Embark (Returns ClientEvent)
    deriving (Read, Typeable, Data)

data WorldPos = WorldPos { world_x ::Double, world_y :: Double } deriving (Data, Typeable, Show, Read)

data Ship = Ship { shipPos :: WorldPos
                 , shipWaypoints :: [WorldPos]
                 } deriving (Data, Typeable, Show, Read)

data ClientEvent = ShipUpdate Ship deriving (Data, Typeable, Show)
