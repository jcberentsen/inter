{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Language.Fay.Yesod
import Data.Data
import Prelude (Read, Int)
#ifdef FAY
import Fay.Text as T
#else
import Data.Text as T
#endif

data Command = StartGame T.Text (Returns T.Text)
    deriving (Read, Typeable, Data)
