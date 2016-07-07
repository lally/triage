{-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
{- DISABLED_ lang uage TemplateHaskell -}
module Home where

import Fay.Yesod
import JQuery
import SharedTypes
import FFI
import Fay.Text

import Prelude ((++), (>>), (>>=), ($), return, fail, putStrLn, show, Bool(True, False), const)

--putStrLn :: String -> Fay ()
--putStrLn = ffi "(function(x) { if (console && console.log) console.log(x) })(%1)"

alert :: Text -> Fay ()
alert = ffi "alert(typeof(%1) + ': ' + JSON.stringify(%1))"

getHead :: Fay ()
getHead = do
  call Head alert

evtHandler ::  Event -> Fay Bool
evtHandler h = do
  alert ("event handler running.")
  getHead
  return False


main :: Fay ()
main = do
  putStrLn (unpack "Got selection ")
  button <- select "#btnPrimary" -- >>= click evtHandler
  putStrLn (show button)
  onClick (\_ ->call (LookupRef "master")  alert >> return False) button
  onClick evtHandler button
  putStrLn (unpack "Set button handler ")
--  return ()
