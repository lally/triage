{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
module Home where

import Fay.Yesod
import JQuery
import SharedTypes
import FFI
import Prelude

alert :: String -> Fay ()
alert = ffi "alert(typeof(%1) + ': ' + JSON.stringify(%1))"

rollDie ∷ Fay ()
rollDie = do call RollDie alert
             return false
main :: Fay ()
main = do
       button ← select ".btn-primary"
       button onClick (
         const $ call RollDie alert >> return False)

