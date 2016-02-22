{-# LANGUAGE RebindableSyntax, OverloadedStrings, TemplateHaskell #-}
module Home where

import Fay.Yesod
import JQuery
import SharedTypes
import FFI
import Prelude

alert :: String -> Fay ()
alert = ffi "alert(typeof(%1) + ': ' + JSON.stringify(%1))"

--rollDie ∷ Fay ()
--rollDie = do call RollDie alert
--             return false

main :: Fay ()
main = do
       select ".btn-primary" >>= onClick (const $ call (LookupRef "head")  alert >> return False)

