module Handler.DocTree where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Data.OrgMode
import Data.Time.LocalTime
import Control.Logging
import Control.Monad.Logger
import Git
import Text.Cassius
import Text.Hamlet
import Text.Julius
import qualified Data.Text as T
import qualified Repo.OrgRepo as OR
import qualified Git.Libgit2 as LG2

-- defaultLayout :: WidgetT site IO () -> HandlerT site IO Html
ndefaultLayout w = do
    p <- widgetToPageContent w
    mmsg <- getMessage
    withUrlRenderer [hamlet|
        $newline never
        $doctype 5
        <html>
            <head>
                <title>#{pageTitle p}
                ^{pageHead p}
            <body>
                $maybe msg <- mmsg
                    <p .message>#{msg}
                ^{pageBody p}
        |]

-- returns ([child nodes], [other children])
splitChildren :: Node -> ([NodeChild], [NodeChild])
splitChildren nd =
  let categorizeNd :: NodeChild -> ([NodeChild], [NodeChild]) -> ([NodeChild], [NodeChild])
      categorizeNd n@(ChildNode _) (ns, os) = (ns ++ [n], os)
      categorizeNd o (ns, os) = (ns, os ++ [o])
  in Import.foldr categorizeNd ([], []) $ nChildren nd

renderChild (ChildText line) = [hamlet| #{tlText line} <br />|]
renderChild (ChildDrawer drawer) = [hamlet| #{drName drawer} <br />|]
renderChild (ChildBabel babel) = [hamlet| |]
renderChild (ChildTable table) = [hamlet| |]

-- Reminder: MonadGit r m -> r=repo m=outer monad.  The outer monad can
-- be IO, but also some logger monad.
renderNode (ChildNode nc) 0 = do
  if (nTopic nc /= "ISSUE EVENTS")
    then do [hamlet|
               <li>#{nTopic nc}
             |]
    else do [hamlet| |]

renderNode (ChildNode nc) n = do
  let nm1 = n - 1
      (cnodes, otherchildren) = splitChildren nc
  if (nTopic nc /= "ISSUE EVENTS")
    then do [hamlet|
               <li>
                 <paper-chip label=#{nTopic nc}>
                   <div class="icon">#{take 1 $ nTopic nc}</div>
                   <h1>#{nTopic nc}
                   <h2>
                     $forall o <- otherchildren
                       ^{renderChild o}
                 <ol>
                    $forall child <- cnodes
                       ^{renderNode child nm1}
             |]
    else do [hamlet| |]

renderNode (ChildDrawer _ ) n = do [hamlet| |]
renderNode (ChildBabel (Babel lines) ) n = do
  [hamlet|
   <prism-js language="bash">
     $forall (TextLine _ t _) <- lines
       #{t}
   |]

renderNode child n = do [hamlet| |]

--
-- |Get child references, up to 'max', of 'commit'.  Each is a pair (sha, msg)
--
refChildren :: (MonadGit r m) => Int -> Commit r -> m [(String, String)]
refChildren max commit = do
  parents <- lookupCommitParents commit
  children <- mapM (refChildren (max-1)) parents
  let current = (T.unpack $ renderObjOid $ commitOid commit, T.unpack $ commitLog commit)
  return $ take max $ current:(concat children)
refChildren 0 commit = return []

--
-- |Redirect to '/doctree/HEAD's SHA/'
--
getHeadR :: Handler Html
getHeadR = do
  app <- getYesod
  let rPath = repoRefPath $ appRepo app
  headSHA <- runStdoutLoggingT $ withRepository LG2.lgFactoryLogger rPath $ do
    ref <- resolveReference "refs/heads/master"
    let refName = maybe "(not found)" show ref
    $(logDebug) $ "getHeadR: HEAD shows up as " ++ (pack $ show refName)
    return refName

  redirect (PathTreeR (T.pack headSHA) [])



getXhrPathTreeR :: Text -> Texts -> Handler TypedContent
getXhrPathTreeR sha path = selectRep $ do
  provideRep $ do
    app <- getYesod
    lineNr <- lookupGetParam "line"
    let line = maybe "no line number" (\n -> "line #" ++ (show n)) lineNr
    let rPath = repoRefPath $ appRepo app
    -- TODO: handle an exception here for a failed parseOid/lookupObject
    (children, obj) <- runStdoutLoggingT $
                        withRepository LG2.lgFactoryLogger rPath $ do
      $(logDebug) "getPathTreeR"
      $(logDebug) $ "Working tree: " ++ (pack $ show rPath)
      $(logDebug) $ "got sha: " ++ sha
      oid <- parseOid sha
      obj <- lookupObject oid
      case obj of
        CommitObj commit -> do
          $(logDebug) $ "  Lookup successful.  Got commit."
          doc <- OR.loadRepoTree commit
          children <- refChildren 30 commit
          return (children, toJSON doc)
        otherwise -> do $(logDebug) $ "  Exists but was not a commit."
                        return ([], object [])
    -- Note: theme is at https://polymerthemes.com/ice/
    let fullPath = T.intercalate "/" path
    return obj

{-eatAndFail :: (CE.Exception e) => e -> HandlerT App IO ([String], [(String, OrgDoc)])
eatAndFail e = do
  putStrLn $ "Failed with exception: " ++ (T.pack $ show e)
  return ([], [])
-}
--
-- | '/doctree/SHA1/path?line=linenum'.  If you go to /doctree raw, you get a
-- redirect to HEAD's SHA1.  Maybe allow a full reference name instead of just
-- the sha.
--
getPathTreeR :: Text -> Texts -> Handler Html
getPathTreeR sha path = do
  app <- getYesod
  lineNr <- lookupGetParam "line"
  let line = maybe "no line number" (\n -> "line #" ++ (show n)) lineNr
  let rPath = repoRefPath $ appRepo app
  -- TODO: handle an exception here for a failed parseOid/lookupObject
  (children, docs) <- runStdoutLoggingT $
                      withRepository LG2.lgFactoryLogger rPath $ do
    $(logDebug) "getPathTreeR"
    $(logDebug) $ "Working tree: " ++ (pack $ show rPath)
    $(logDebug) $ "got sha: " ++ sha
    oid <- parseOid sha
    obj <- lookupObject oid
    case obj of
      CommitObj commit -> do
        $(logDebug) $ "  Lookup successful.  Got commit."
        docs <- OR.loadRepo commit
        children <- refChildren 30 commit
        return (children, docs)
      otherwise -> do $(logDebug) $ "  Exists but was not a commit."
                      return ([], [])
  -- Note: theme is at https://polymerthemes.com/ice/
  let fullPath = T.intercalate "/" path
      shaAbbrev = T.take 8 sha
      docTitle = shaAbbrev `T.append` fullPath
      fullDocTitle = "DocTree: " `T.append` docTitle
    -- TODOs here:
    --  (1) DONE Move to a template file(!)
    --  (2) Add JS to enable pop-ups of the cards when clicked.
    --    a. Should raise the z-order
    --    b. Should pop-down any other cards that are up.

    --  (3) Find someplace to save state for polymer <link rel=import>
    --      includes.  The script should include facilities for
    --      unique'ing these.  There are other uniquing facilities in
    --      Yesod, but nothing for this type of inclusion.
  defaultLayout $ do
      setTitle "Foo"
      toWidgetBody $(hamletFile "templates/doctree-body.hamlet")
      toWidgetHead $(hamletFile "templates/doctree-head.hamlet")
      addScriptRemote "/static/jquery-1.10.2.min.js"
      addScriptRemote "/static/d3.v3.min.js"
--      toWidget $ $(juliusFile "templates/doctree-body.julius")
      addScriptRemote "/static/js-devel/doctree.js"
--      addScriptRemote "/static/text_object.js"
      addStylesheetRemote "/static/styles/doctree.css"
      {-
      let remotes = [
            "/polymer-themes/ice.html",
            "/paper-scroll-header-panel/paper-scroll-header-panel.html",
            "/paper-toolbar/paper-toolbar.html",
            "/paper-card/paper-card.html",
            "/paper-icon-button/paper-icon-button.html",
            "/paper-drawer-panel/paper-drawer-panel.html",
            "/paper-chip/paper-chip.html",
            "/iron-icons/iron-icons.html",
            "/iron-icon/iron-icon.html",
            "/iron-flex-layout/classes/iron-flex-layout.html"
            ]
      mapM_ addScriptRemote $ map (\s -> "/static/bower_components" ++ s) remotes -}

