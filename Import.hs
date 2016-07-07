{-# LANGUAGE CPP, OverloadedStrings #-}
module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

import Fay
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

modConfig ∷ Config → Config
modConfig c = c { configSourceMap = True, configPrettyPrint = True }

fayFile' :: Exp -> FayFile
fayFile' staticR moduleName
    | development = fayFileReloadWithConfig 'modConfig settings
    | otherwise   = fayFileProd settings
  where
    settings =
      let defaults = (yesodFaySettings moduleName)
      in defaults
        { yfsSeparateRuntime = Just ("static", staticR)
        -- , yfsPostProcess = readProcess "java" ["-jar", "closure-compiler.jar"]
        , yfsPackages = (yfsPackages defaults) ++ ["fay-jquery","fay-text"]
        , yfsTypecheckDevel = True
        , yfsExternal = Just ("static", staticR)
        }
