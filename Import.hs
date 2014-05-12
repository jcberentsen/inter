module Import
    ( module Import
    ) where


import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

import Yesod.Fay
import Language.Haskell.TH.Syntax (Exp)
--import System.Process (readProcess)

fayFile' :: Exp -> FayFile
fayFile' staticR moduleName
    | development = fayFileReload fay_settings
    | otherwise   = fayFileProd fay_settings
  where
    fay_settings = (yesodFaySettings moduleName)
        { yfsSeparateRuntime = Just ("static", staticR)
          -- , yfsPostProcess = readProcess "java" ["-jar", "closure-compiler.jar"]
        , yfsExternal = Just ("static", staticR)
        , yfsPackages = ["fay-jquery", "fay-ref", "fay-base", "fay-text"]
        }
