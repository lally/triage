module Handler.Scratch where
import Import
import Control.Logging
import Control.Monad.Logger
import Language.Haskell.TH
import Text.Hamlet

getScratchR ∷ Handler Html
getScratchR = do
  defaultLayout $ do
    setTitle "Welcome To Yesod!"
    $(fayFile' (ConE 'StaticR) "Home")
    $(widgetFile "scratch")
--    $(widgetFile "homepage")

postScratchR ∷ Handler Html
postScratchR = do
  defaultLayout $ do
    setTitle "Welcome To Yesod!"
---    $(widgetFile "homepage")
