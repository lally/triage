module Handler.DocTree where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Data.Char (isSpace)
import Data.OrgMode
--import Data.Text.Encoding (encodeUtf8)
import Data.Time.Format
import Data.Time.Locale.Compat
import Data.Time.LocalTime
import Control.Logging
import Control.Monad.Logger
import Git
import Text.Blaze
import Text.Cassius
import Text.Hamlet
import Text.Julius
import qualified Control.Monad as CM
import qualified Data.Text as T
import qualified Data.List as L
import qualified Prelude as P
import qualified Git.Libgit2 as LG2
import qualified Repo.OrgRepo as OR


--
-- |Get child references, up to 'max', of 'commit'.  Each is a pair
-- (sha, msg, date)
--
refChildren :: (MonadGit r m) => Int -> Commit r -> m [(String, String, String)]
refChildren max commit = do
  parents <- lookupCommitParents commit
  children <- mapM (refChildren (max-1)) parents
  let timestamp = formatTime defaultTimeLocale "%m/%d" $
                  signatureWhen $ commitCommitter commit
  let current = (T.unpack $ renderObjOid $ commitOid commit,
                 T.unpack $ commitLog commit,
                 timestamp)
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

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

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
        _ -> do $(logDebug) $ "  Exists but was not a commit."
                return ([], object [])
    -- Note: theme is at https://polymerthemes.com/ice/
    let fullPath = T.intercalate "/" path
    return obj

{-
eatAndFail :: (CE.Exception e) => e
              -> HandlerT App IO ([String], [(String, OrgDoc)])
eatAndFail e = do
  putStrLn $ "Failed with exception: " ++ (T.pack $ show e)
  return ([], [])
-}

--inRepo :: (MonadGit r m) => Text -> (Commit r -> ReaderT (LG2.LgRepo (LoggingT (HandlerT App IO))) (Oid LG2.LgRepo) a) -> Handler (Maybe a)
inRepo
  :: (MonadMask m, MonadBaseControl IO m, MonadHandler m,
      HandlerSite m ~ App) =>
     Text
     -> (Commit LG2.LgRepo -> ReaderT LG2.LgRepo (LoggingT m) a)
     -> m (Maybe a)
inRepo sha action = do
  app <- getYesod
  let rPath = repoRefPath $ appRepo app
  runStdoutLoggingT $ withRepository LG2.lgFactoryLogger rPath $ do
    oid <- parseOid sha
    obj <- lookupObject oid
    case obj of
      CommitObj commit -> do
        res <- action commit
        return $ Just res
      _ -> return Nothing


mfilter pred r@(Just a) = if pred a then r else Nothing
mfilter pred Nothing = Nothing

getDocR :: Text -> Texts -> Handler Html
getDocR sha path = do
  rawNodePath <- lookupGetParam "path"
  let nodePath = maybe [] (
        \n -> map (P.floor . P.read . T.unpack) $
              T.splitOn "," n) $ mfilter (
        \l -> T.length l > 0) rawNodePath
  docText <- inRepo sha (OR.getDoc $ encodeUtf8 $T.intercalate "/" path)
  let doc = orgFile <$> docText
      -- This is a lens.
      isChildNode (ChildNode nd) = Just nd
      isChildNode _ = Nothing
      mkMaybe pred val = if pred then Just val else Nothing
      getnth n list = mkMaybe (n >= 0 && length list > n) (
        P.head $ P.drop n list)
      traversePath :: [Int] -> Node -> Maybe Node
      traversePath (p:ps) node =
        let children = nChildren node
            index = p-1
            childNodes = mapMaybe isChildNode children
        in CM.join $ traversePath <$> (pure ps) <*> (getnth index childNodes)
      traversePath [p] node =
        getnth (p-1) $ mapMaybe isChildNode (nChildren node)
      traversePath [] node = Just node
      docNodes = odNodes <$> doc
      fakeRoot children = Node 0 Nothing [] (map ChildNode children) "" (
        TextLine 0 "" Nothing)
      -- We're creating a fake root, so we have to prepend a 1 to the
      -- path to go through the root.
      nodeAtPath = CM.join $ traversePath <$> (pure (1:nodePath)) <*> (
        fmap fakeRoot docNodes)
      nodePresentation =
        (\node -> L.intercalate "\n" $ map tlText $ getTextLines node) <$> nodeAtPath
  defaultLayout $ do
    toWidgetBody $ [hamlet|
                    $maybe preso <- nodePresentation
                       <pre .presoText>#{preso}
                          |]

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
  (children, docs, commitMsg) <- runStdoutLoggingT $
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
        return (children, docs, T.unpack $ commitLog commit)
      _ -> do $(logDebug) $ "  Exists but was not a commit."
              return ([], [], "(not a commit)")
  -- Note: theme is at https://polymerthemes.com/ice/
  let fullPath =
        if (length path) > 0
        then ":" `T.append` (T.intercalate "/" path)
        else ""
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
      setTitle (text fullDocTitle)
      addScriptRemote "/static/jquery-1.10.2.min.js"
      addScriptRemote "/static/d3.v3.min.js"
      addStylesheetRemote "/static/styles/doctree.css"
      addStylesheetRemote "/static/styles/shelf.css"
      toWidgetBody $(hamletFile "templates/doctree-body.hamlet")
      toWidgetHead $(hamletFile "templates/doctree-head.hamlet")
      addScriptRemote "/static/js-devel/doctree.js"
      toWidget $ $(juliusFile "templates/doctree-body.julius")
      addScriptRemote "/static/js-devel/shelf.js"
--      addScriptRemote "/static/text_object.js"
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

