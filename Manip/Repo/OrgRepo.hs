module Manip.Repo.OrgRepo where
import Data.Aeson
import Data.Maybe
import Data.Tagged
import Git
import Control.Monad.Logger

-- TODO: split out Issues from org-issue-sync for inclusion here.
-- OR, just make org-issue-sync a dependency.
import Import as P
--import Debug.Trace
import qualified Data.List as L
import qualified Data.OrgMode as OM
import qualified Data.Issue as DI
import qualified Data.OrgIssue() -- as DOI
import qualified Data.IssueJSON() -- as IJ
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.List as CL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Git.Libgit2 as LG2

tpack :: String -> T.Text
tpack = T.pack

tap ∷ T.Text → T.Text → T.Text
tap = T.append

data DocTreePath = DocTreePath
                   { pRevisionHash :: Text
                     -- ^ a SHA1
                   , pFilePath :: [Text]
                   , pNodePath :: [Double]
                     -- ^ Child indices in trees.  The empty list is
                     -- the root.  The nth child is [n].
                   } deriving (Eq, Show)

instance ToJSON DocTreePath where
  toJSON (DocTreePath hsh path nodep) =
    String (hsh `tap` (T.intercalate "/" path) `tap` "?path=" `tap` (
               tpack $ L.intercalate "," $ map show nodep))

-- |OrgNode/Text/Table/Source correspond to OrgMode NodeChild types,
-- with Source meaning Babel.  TKEntity are any kind of embedded
-- entities, like Issues.  TKSource's arg is a language name, if known.
-- TODO: May not need this anymre
data DocTreeKind = TKDir | TKOrgNode | TKText | TKTable | TKSource (Maybe String)
                 | TKEntity String deriving (Eq, Show)

data DocTreeData = DDText Text
                 | DDTable OM.Table
                 | DDSource OM.Babel
                 | DDIssue DI.Issue -- ^ a placeholder.
                 deriving (Eq, Show)

data DocTreeEntity = DTIssue DI.Issue
                     | DTOrg OM.Node
                     deriving (Eq, Show)

instance OM.NodeUpdate DocTreeEntity where
  -- findItemInNode :: OM.Node -> Maybe DocTreeEntity
  findItemInNode n =
    let asIssue = DTIssue <$> OM.findItemInNode n
    in if isJust asIssue then asIssue else (Just $ DTOrg n)

  -- updateNodeLine :: DocTreeEntity -> OM.Node -> Maybe OM.Node
  updateNodeLine (DTIssue i) n = OM.updateNodeLine i n
  updateNodeLine (DTOrg _) n = Just n

-- |A graph node.  Data in 'tData' will go into the expanded card of
-- this node.  Children will show up as suboordinate nodes.  This
-- element, and its children, need not represent the entire underlying
-- entity, just enough for useful display and interaction.
data DocTreeEntry = DocTreeEntry
                    { tTitle :: Text
                    , tPath :: DocTreePath
                    , tChildren :: [DocTreeEntry]
                    , tCompressedEntries :: [(Text, DocTreeKind)]
                    , tData :: [DocTreeData]
                    , tKind :: DocTreeKind
                    , tTags :: [Text]
                    , tState :: Maybe Text
                    , tEntity :: Maybe DocTreeEntity
                    } deriving (Eq, Show)

instance ToJSON DocTreeData where
  toJSON (DDText text) = String text
  toJSON (DDTable _) = object []
  toJSON (DDSource _) = object []
  toJSON (DDIssue _) = object []

instance ToJSON DocTreeKind where
  toJSON TKDir = String "directory"
  toJSON TKOrgNode = String "org"
  toJSON TKText = String "text"
  toJSON TKTable = String "table"
  toJSON (TKSource (Nothing)) = String "src"
  toJSON (TKSource (Just s)) = String ("src-" ++ (T.pack s))
  toJSON (TKEntity ty) = String (T.pack ty)

instance ToJSON DocTreeEntry where
  toJSON ent = object (
               [ "name" .= tTitle ent
               , "path" .= tPath ent
               , "data" .= tData ent
               , "children" .= tChildren ent
               , "kind" .= tKind ent
               , "tags" .= tTags ent
               , "state" .= maybe "OPEN" id (tState ent)
               ] ++ maybe [] (\v -> [("value", toJSON v)]) (tEntity ent))

instance ToJSON DocTreeEntity where
  toJSON (DTIssue iss) = toJSON iss
  toJSON (DTOrg _) = object []

fixDecodeErr :: String -> Maybe Word8 -> Maybe Char
fixDecodeErr _ (Just _) = Just '-'
fixDecodeErr _ Nothing = Nothing

getContents :: (MonadGit r m) => TreeFilePath -> TreeEntry r -> m T.Text
getContents name entry = do
  ty <- case entry of
    BlobEntry oid _ -> do
      blob <- lookupBlob oid
      value <- case (blobContents blob) of
        BlobString str -> return $ TE.decodeUtf8With fixDecodeErr str
        BlobStringLazy _ -> return "lazy blobstring"
        BlobStream _ -> return "blobstream"
        BlobSizedStream _ _ -> return "blobstream w/len "
      return value
    TreeEntry oid -> do
      return $ "tree: " `T.append` (renderObjOid oid)
    CommitEntry _ -> do
      return "commit"
  return $ (TE.decodeUtf8With fixDecodeErr name) `T.append` ": " `T.append` ty

readBlobContents :: (MonadGit r m) => BlobContents m -> m String
readBlobContents (BlobString str) =
  do return $ T.unpack $ TE.decodeUtf8With fixDecodeErr str
readBlobContents (BlobStringLazy _) =fail "readBlobContents (BlobStringLazy) not defined yet."
                                     -- undefined
{-  do
  let decoded = TE.decodeUtf8 str
  return $ T.unpack decoded -> -}
readBlobContents (BlobStream src) = do
  str <- src $$ CL.consume
  return $ T.unpack $ TE.decodeUtf8With fixDecodeErr $ BS.concat str
readBlobContents (BlobSizedStream _ _) = fail "readBlobContents (BlobSizedStream) undefined"  -- !(ByteSource m) !Int

getRecursiveContents :: (MonadGit r m, MonadLogger m) =>
                        (TreeFilePath, TreeEntry r)
                        -> m [(TreeFilePath, OM.OrgDoc)]
getRecursiveContents (name, (BlobEntry oid (PlainBlob))) = do
  -- TODO: put in filter to take out .gitignore, etc.
  let entryName = L.last $ BS8.split '/' name
      suppressed = not (isSuffixOf ".org" entryName)
  res <- if suppressed
         then do return []
         else do blob <- lookupBlob oid
                 let contents = blobContents blob
                 text <- readBlobContents contents
                 return [(name, OM.orgFile text)]
  return res

getRecursiveContents (_, (BlobEntry _ (ExecutableBlob))) = return []
getRecursiveContents (_, (BlobEntry _ (SymlinkBlob))) = return []
getRecursiveContents (_, (TreeEntry _)) = return []
{- do
  tree <- lookupTree oid
  entries <- sourceTreeEntries tree $$ CL.consume
  contents <- mapM getRecursiveContents entries
  let prefixify (p, e) = (name P.++ "/:" P.++ p, e)
  return $ P.map prefixify $ L.concat contents
-}
getRecursiveContents (name, (CommitEntry oid)) = do
  commit <- lookupCommit oid
  tree <- lookupTree (commitTree commit)
  entries <- sourceTreeEntries tree $$ CL.consume
  $(logDebug) $ "Got contents: " ++ (T.pack $ L.intercalate "\n" (map (show . fst)  entries))
  contents <- mapM getRecursiveContents entries
  let prefixify (p, e) = (name P.++ p, e)
  return $ P.map prefixify $ L.concat contents
--  getRecursiveContents (name, TreeEntry $ commitTree commit)

getDoc :: (MonadGit r m, MonadLogger m) => ByteString -> Commit r -> m String
getDoc path commit = do
  tree <- lookupTree (commitTree commit)
  ent <- treeEntry tree path
  $(logDebug) $ "getDoc: " ++  (decodeUtf8 path)
  case ent of
    (Just (BlobEntry oid _)) -> do
        blob <- lookupBlob oid
        let contents = blobContents blob
        readBlobContents contents
    (Just (TreeEntry _)) -> return "A TreeEntry"
    (Just (CommitEntry _)) -> return "CommitEntry"
--    (Just _) -> return "no match for stored type."
    Nothing -> return "nothing"


loadRepo :: (MonadGit r m, MonadLogger m) =>
            Commit r
            -> m [(String, OM.OrgDoc)]
loadRepo commit = do
  contents <- getRecursiveContents ("/", CommitEntry (commitOid commit))
  let xlateName (p,d) = (BS8.unpack p, d)
  return $ P.map xlateName contents

empty :: [a] -> Bool
empty [] = True
empty _ = False

nodeChildren :: [OM.NodeChild] -> ([OM.Node], [DocTreeData])
nodeChildren children =
      -- Note that this is a hack here, and should get killed off once
      -- we properly have OrgIssue recognition.
  let isNode (OM.ChildNode n)
        | OM.nTopic n == "ISSUE EVENTS" = Nothing
        | otherwise = Just n
      isNode _ = Nothing
      nodeString (OM.ChildNode _) = Nothing
      nodeString (OM.ChildText ln) =
        Just (DDText $ T.pack $ OM.tlText ln)
      nodeString (OM.ChildDrawer d) =
        Just (DDText $ T.pack $ concatMap OM.tlText $ OM.drLines d)
      nodeString (OM.ChildBabel b) = Just (DDSource b)
      nodeString (OM.ChildTable t) = Just (DDTable t)
  in (mapMaybe isNode children, mapMaybe nodeString children)

makeDocEntry :: DocTreePath -> OM.Node -> DocTreeEntry
makeDocEntry parent node =
  let (rawnodes, dat) = nodeChildren (OM.nChildren node)
      prefixText = fmap (\(OM.Prefix s) -> T.pack s) $ OM.nPrefix node
      getStatusText (DI.Open) = "OPEN"
      getStatusText (DI.Active) = "ACTIVE"
      getStatusText (DI.Closed) = "CLOSED"
      statusText iss = Just (getStatusText $ DI.status iss)
      indexedNodes = zip [1..] rawnodes
      makeChildEntry ∷ (Int, OM.Node) → DocTreeEntry
      makeChildEntry (i, cnode) =
        let childPath = parent { pNodePath = (pNodePath parent) ++ [fromIntegral i] }
        in makeDocEntry childPath cnode
  in case OM.findItemInNode node of
    (Just (DTIssue iss)) -> DocTreeEntry (T.pack $ DI.summary iss) parent [] [] [] (
      TKEntity "issue") (map T.pack $ DI.tags iss) (statusText iss) (
      Just $ DTIssue iss)
    _ -> DocTreeEntry (T.pack $ OM.nTopic node) parent (
      map makeChildEntry indexedNodes) [] dat TKOrgNode (
      map T.pack $ OM.nTags node) prefixText Nothing

convertDoc :: [Text] -> DocTreePath -> OM.OrgDoc -> DocTreeEntry
convertDoc path parent doc =
  let sha = pRevisionHash parent
      title =
        let titles = filter (\p -> OM.fpName p == "TITLE") $ OM.odProperties doc
        in case titles of
          [OM.OrgFileProperty _ v] -> T.pack v
          _ -> L.last path
  in DocTreeEntry title (DocTreePath sha path []) (
    map (makeDocEntry parent) (OM.odNodes doc)) [] [] TKOrgNode [] Nothing Nothing

updateTree' :: Int -> DocTreeEntry -> [Text] -> OM.OrgDoc -> DocTreeEntry
updateTree' n ent path doc =
  let p = tPath ent
      c = tChildren ent
--      d = tData ent
--      k = tKind ent
      premain = drop n path
  in if empty premain
     then ent { tChildren = (convertDoc path p doc):c }
     else let matchingChild ccld =
                let fp = drop n $ pFilePath $ tPath ccld
                in L.head fp == L.head premain
{-              intNodeName =
                if (T.length $ L.head premain) > 0
                then L.head premain
                else L.head $ dropWhile (\n -> T.length n < 1) $ reverse path -}
              (pfx, cld, sfx) =
                case L.findIndex matchingChild c of
                     (Just i) -> let (f, hd) = splitAt i c
                                     repl = (updateTree' (n+1) (L.head hd)
                                             path doc)
                                 in (f, repl, drop (i+1) c)
                     Nothing -> -- create intermediate node
                       let intNode = DocTreeEntry {
                             tTitle = (L.head $ drop n path),
                             tPath = DocTreePath {
                               pRevisionHash = pRevisionHash p,
                               pFilePath = pFilePath p ++ [L.head premain],
                               pNodePath = [] },
                             tChildren = [],
                             tCompressedEntries = [],
                             tData = [],
                             tKind = TKDir,
                             tTags = [],
                             tEntity = Nothing,
                             tState = Nothing
                             }
                       in (c, updateTree' (n+1) intNode path doc, [])
          in ent { tChildren = pfx ++ (cld:sfx) }

updateTree :: DocTreeEntry -> ([Text], OM.OrgDoc) -> DocTreeEntry
updateTree ent (path, doc) =
  updateTree' 0 ent path doc

-- | Returns either a merged entry, or Nothing if they can't be merged. 
mergeEntries :: DocTreeEntry -> DocTreeEntry -> DocTreeEntry
mergeEntries parent child =
  let pathelem = case tKind parent of
        TKDir -> L.last $ pFilePath $ tPath $ parent
        TKOrgNode -> tTitle parent
        _ → "<unknown>"
      combinedTitle = if (not $ T.null $ tTitle parent)
                      then if (not $ T.null $ tTitle child)
                           then (tTitle parent `T.append` "/"
                                 `T.append` tTitle child)
                           else tTitle parent
                      else tTitle child

      eitherTitle = if T.null (tTitle child)
                    then tTitle parent
                    else tTitle child
      resultTitle = if tKind parent == TKDir && tKind child == TKOrgNode
                    then eitherTitle
                    else combinedTitle
  in child { tCompressedEntries =
                (pathelem, tKind parent):(tCompressedEntries parent),
             tTitle = resultTitle}

-- Finds entries with just 1 child and:
--  (a) combines the parent and child
--  (b) recurses on the result
compressTree :: DocTreeEntry -> DocTreeEntry
compressTree input =
  case tChildren input of
    [] -> input
    [child] -> compressTree (mergeEntries input child)
    _ -> input {
      tChildren = map compressTree (tChildren input) }

{-loadRepoRoot ∷ (MonadGit r m, MonadLogger m, MonadCatch m,
                MonadMask m, MonadIO m, MonadBaseControl IO m) ⇒ RepoRef → Text → m (Maybe DocTreeEntry) -}
loadRepoRoot ∷ (MonadLogger m,
                MonadMask m,
                MonadIO m,
                MonadBaseControl IO m) ⇒ RepoRef → Text → m (Maybe DocTreeEntry)
loadRepoRoot repo sha = do
  let rPath = repoRefPath repo
  ret ← runStdoutLoggingT $ withRepository LG2.lgFactoryLogger rPath $ do
    oid ← parseOid sha
    obj ← lookupObject oid
    case obj of
      CommitObj commit → do
        doc ← loadRepoTree commit
        return (Just doc)
      _ → return Nothing
  return ret

loadRepoTree :: (MonadGit r m, MonadLogger m) => Commit r -> m DocTreeEntry
loadRepoTree commit = do
  -- This is a list of (TreeFilePath, OrgDoc).  Pull all the paths out
  -- and construct a tree for the ancestor directory structure.
  contents <- getRecursiveContents ("/", CommitEntry (commitOid commit))
  let oid = commitOid commit
      sha = renderOid $ unTagged oid
      makePath (path, doc) = (T.split (=='/') $
                                 TE.decodeUtf8With fixDecodeErr path, doc)
        -- (T.split (=='/') path, doc)
  -- get SHA of commit
  let root = (DocTreeEntry sha (DocTreePath sha [] []) [] [] [] TKDir []
              Nothing Nothing)
      finalTree = foldl' updateTree root $ map makePath contents
      -- err: contents is [(TreeFilePath, OM.OrgDoc)], not [(Text,...)]
  return $ compressTree finalTree

getHeadSha ∷ RepoRef → IO (String)
getHeadSha ref = do
  let rPath = repoRefPath ref
  headSHA <- runStdoutLoggingT $ withRepository LG2.lgFactoryLogger rPath $ do
    rref <- resolveReference "refs/heads/master"
    let refName = maybe "(not found)" show rref
    $(logDebug) $ "getHeadSHA: HEAD shows up as " ++ (pack $ show refName)
    return refName
  return headSHA
  
--
-- |Get child references, up to 'max', of 'commit'.  Each is a pair
-- (sha, msg, date)
--
refChildren :: (MonadGit r m) => Int -> Commit r -> m [(String, String, String)]
refChildren 0 _ = return []
refChildren mx commit = do
  parents <- lookupCommitParents commit
  children <- mapM (refChildren (mx-1)) parents
  let timestamp = formatTime defaultTimeLocale "%m/%d" $
                  signatureWhen $ commitCommitter commit
  let current = (T.unpack $ renderObjOid $ commitOid commit,
                 T.unpack $ commitLog commit,
                 timestamp)
  return $ take mx $ current:(concat children)

