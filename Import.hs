{-# LANGUAGE CPP #-}
module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

import Yesod.Fay
import Language.Haskell.TH.Syntax (Exp)
-- import System.Process (readProcess)

-- | In a standard scaffolded site, @development@ is provided by
-- @Settings.Development@.
development :: Bool
#ifdef PRODUCTION
development = False
#else
development = True
#endif

fayFile' :: Exp -> FayFile
fayFile' staticR moduleName
    | development = fayFileReload settings
    | otherwise   = fayFileProd settings
  where
    settings =
      let defaults = (yesodFaySettings moduleName)
      in defaults
        { yfsSeparateRuntime = Just ("static", staticR)
        -- , yfsPostProcess = readProcess "java" ["-jar", "closure-compiler.jar"]
        , yfsPackages = (yfsPackages defaults) ++ ["fay-jquery","fay-text"]
        , yfsExternal = Just ("static", staticR)
        }
