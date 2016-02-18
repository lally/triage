{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Prelude
import Fay.Yesod
import Data.Data
import Data.Text
#ifdef FAY
import FFI
#else
import Fay
import Fay.FFI
#endif

data Command = LookupRef Text (Returns Text)
    deriving (Read, Typeable, Data)
