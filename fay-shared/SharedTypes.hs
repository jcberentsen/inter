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
    deriving (Read, Typeable, Data)

type WorldPos = (Double, Double)

data ClientEvent = NewDestination WorldPos deriving (Data, Typeable, Show)
