{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module Home where

import Fay.Yesod
import JQuery
import SharedTypes
import FFI
import Prelude

alert :: String -> Fay ()
alert = ffi "alert(typeof(%1) + ': ' + JSON.stringify(%1))"


main :: Fay ()
main = void $ select ".btn-primary" >>= onClick (const $ call RollDie alert >> return False)

