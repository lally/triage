module Handler.DocTree where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Control.Logging
import Control.Monad.Logger
import Git
import Git.Libgit2

getTreeR :: Handler Html
getTreeR = do
  app <- getYesod
  let rPath = repoRefPath $ appRepo app
  headSHA <- runStdoutLoggingT $ withRepository lgFactoryLogger rPath $ do
    $(logDebug) "This is a debug log message"
    $(logDebug) $ "Working tree: " ++ (pack $ show rPath)
    ref <- resolveReference "refs/heads/master"
    let refName = maybe "(not found)" show ref
    $(logDebug) $ " HEAD shows up as " ++ (pack $ show refName)
    return $ show refName
  defaultLayout $ do
    setTitle "DocTree here"
    $(widgetFile "tree")
