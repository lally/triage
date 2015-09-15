module Repo.OrgRepo where
import Data.Aeson
import Data.Maybe
import Data.Tagged
import Git
-- TODO: split out Issues from org-issue-sync for inclusion here.
-- OR, just make org-issue-sync a dependency.
--import Data.Issue

import Import as P
import Debug.Trace
import qualified Data.List as L
import qualified Data.OrgMode as OM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.List as CL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
tpack :: String -> T.Text
tpack = T.pack

tap = T.append

data DocTreePath = DocTreePath
                   { pRevisionHash :: Text
                     -- ^ a SHA1
                   , pFilePath :: [Text]
                   , pLineNumber :: Maybe Int
                     -- ^ |Just n| if this is an entry in a file. Nothing if a dir.
                   } deriving (Eq, Show)

instance ToJSON DocTreePath where
  toJSON (DocTreePath hash path Nothing) =
    String (hash `tap` ":" `tap` (T.intercalate "/" path))
  toJSON (DocTreePath hash path (Just n)) =
    String (hash `tap` ":" `tap` (T.intercalate "/" path) `tap` "#" `tap` (tpack$ show n))

-- |OrgNode/Text/Table/Source correspond to OrgMode NodeChild types,
-- with Source meaning Babel.  TKEntity are any kind of embedded
-- entities, like Issues.  TKSource's arg is a language name, if known.
-- TODO: May not need this anymre
data DocTreeKind = TKDir | TKOrgNode | TKText | TKTable | TKSource (Maybe String)
                 | TKEntity String deriving (Eq, Show)

data DocTreeData = DDText Text
                 | DDTable OM.Table
                 | DDSource OM.Babel
                 | DDIssue Text -- ^ a placeholder.
                 deriving (Eq, Show)

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
                    } deriving (Eq, Show)

instance ToJSON DocTreeData where
  toJSON (DDText text) = String text
  toJSON (DDTable table) = object []
  toJSON (DDSource babel) = object []
  toJSON (DDIssue text) = object []

instance ToJSON DocTreeKind where
  toJSON TKDir = String "directory"
  toJSON TKOrgNode = String "org"
  toJSON TKText = String "text"
  toJSON TKTable = String "table"
  toJSON (TKSource (Nothing)) = String "src"
  toJSON (TKSource (Just s)) = String ("src-" ++ (T.pack s))
  toJSON (TKEntity ty) = String (T.pack ty)

instance ToJSON DocTreeEntry where
  toJSON ent = object
               [ "name" .= tTitle ent
               , "path" .= tPath ent
               , "value" .= tData ent
               , "children" .= tChildren ent
               , "kind" .= tKind ent
               ]

getContents :: (MonadGit r m) => TreeFilePath -> TreeEntry r -> m T.Text
getContents name entry = do
  ty <- case entry of
    BlobEntry oid kind -> do
      blob <- lookupBlob oid
      value <- case (blobContents blob) of
        BlobString str -> return $ TE.decodeUtf8 str
        BlobStringLazy str -> return "lazy blobstring"
        BlobStream bytestrc -> return "blobstream"
        BlobSizedStream bytesrc len -> return "blobstream w/len "
      return value
    TreeEntry oid -> do
      return $ "tree: " `T.append` (renderObjOid oid)
    CommitEntry oid -> do
      return "commit"
  return $ (TE.decodeUtf8 name) `T.append` ": " `T.append` ty

readBlobContents :: (MonadGit r m) => BlobContents m -> m String
readBlobContents (BlobString str) = return $ T.unpack $ TE.decodeUtf8 str
readBlobContents (BlobStringLazy str) = undefined
{-  do
  let decoded = TE.decodeUtf8 str
  return $ T.unpack decoded -> -}
readBlobContents (BlobStream src) = do
  str <- src $$ CL.consume
  return $ T.unpack $ TE.decodeUtf8 $ BS.concat str
readBlobContents (BlobSizedStream src len) = undefined -- !(ByteSource m) !Int

getRecursiveContents :: (MonadGit r m) => (TreeFilePath, TreeEntry r)
                        -> m [(TreeFilePath, OM.OrgDoc)]
getRecursiveContents (name, (BlobEntry oid (PlainBlob))) = do
  blob <- lookupBlob oid
  let contents = blobContents blob
  text <- readBlobContents contents
  return [(name, OM.orgFile text)]

getRecursiveContents (name, (BlobEntry oid (ExecutableBlob))) = return []
getRecursiveContents (name, (BlobEntry oid (SymlinkBlob))) = return []
getRecursiveContents (name, (TreeEntry oid)) = return []
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
  contents <- mapM getRecursiveContents entries
  let prefixify (p, e) = (name P.++ p, e)
  return $ P.map prefixify $ L.concat contents
--  getRecursiveContents (name, TreeEntry $ commitTree commit)

loadRepo :: (MonadGit r m) => Commit r -> m [(String, OM.OrgDoc)]
loadRepo commit = do
  contents <- getRecursiveContents ("/", CommitEntry (commitOid commit))
  let xlateName (p,d) = (BS8.unpack p, d)
  return $ P.map xlateName contents

-- |Returns whether 'a' is a child of 'b'
childOf :: DocTreePath -> DocTreePath -> Bool
childOf a b@(DocTreePath hash path Nothing) = undefined
childOf a b@(DocTreePath hash path (Just nr)) = undefined

empty :: [a] -> Bool
empty [] = True
empty _ = False

nodeChildren :: [OM.NodeChild] -> ([OM.Node], [DocTreeData])
nodeChildren children =
  let isNode (OM.ChildNode n)
        | OM.nTopic n == "ISSUE EVENTS" = Nothing
        | otherwise = Just n
      isNode _ = Nothing
      nodeString (OM.ChildNode n) = Nothing
      nodeString (OM.ChildText ln) =
        Just (DDText $ T.pack $ OM.tlText ln)
      nodeString (OM.ChildDrawer d) =
        Just (DDText $ T.pack $ concatMap OM.tlText $ OM.drLines d)
      nodeString (OM.ChildBabel b) = Just (DDSource b)
      nodeString (OM.ChildTable t) = Just (DDTable t)
  in (mapMaybe isNode children, mapMaybe nodeString children)

makeDocEntry :: DocTreePath -> OM.Node -> DocTreeEntry
makeDocEntry parent node =
  let line = OM.tlLineNum $ OM.nLine node
      path = DocTreePath (pRevisionHash parent) (pFilePath parent) line
      (rawnodes, dat) = nodeChildren (OM.nChildren node)
  in DocTreeEntry (T.pack $ OM.nTopic node) path (
    map (makeDocEntry path) rawnodes) [] dat TKOrgNode

convertDoc :: [Text] -> DocTreePath -> OM.OrgDoc -> DocTreeEntry
convertDoc path parent doc =
  let sha = pRevisionHash parent
      title =
        let titles = filter (\p -> OM.fpName p == "TITLE") $ OM.odProperties doc
        in case titles of
          [OM.OrgFileProperty n v] -> T.pack v
          otherwise -> L.last path
  in DocTreeEntry title (DocTreePath sha path Nothing) (
    map (makeDocEntry parent) (OM.odNodes doc)) [] [] TKOrgNode

updateTree' :: Int -> DocTreeEntry -> [Text] -> OM.OrgDoc -> DocTreeEntry
updateTree' n ent@(DocTreeEntry _ p c _ d k) path doc =
  let premain = drop n path
  in if empty premain
     then ent { tChildren = (convertDoc path p doc):c }
     else let matchingChild cld =
                let fp = drop n $ pFilePath $ tPath cld
                in L.head fp == L.head premain
              intNodeName = if (T.length $ L.head premain) > 0
                            then L.head premain
                            else L.head $ dropWhile (\n -> T.length n < 1) $ reverse path
              {-pfx :: [DocTreeEntry]
              cld :: DocTreeEntry
              sfx :: [DocTreeEntry]-}
              (pfx, cld, sfx) =
                case L.findIndex matchingChild c of
                     (Just i) -> let (f, hd) = splitAt i c
                                     repl = updateTree' (n+1) (L.head hd) path doc
                                 in (f, repl, drop (i+1) c)
                     Nothing -> -- create intermediate node
                       let intNode = DocTreeEntry {
                             tTitle = (L.head $ drop n path),
                             tPath = DocTreePath {
                               pRevisionHash = pRevisionHash p,
                               pFilePath = pFilePath p ++ [L.head premain],
                               pLineNumber = Nothing },
                             tChildren = [],
                             tCompressedEntries = [],
                             tData = [],
                             tKind = TKDir }
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
      title = if T.null (tTitle child) then tTitle parent else tTitle child
  in child { tCompressedEntries =
                (pathelem, tKind parent):(tCompressedEntries parent),
             tTitle = title}

-- Finds entries with just 1 child and:
--  (a) combines the parent and child
--  (b) recurses on the result
compressTree :: DocTreeEntry -> DocTreeEntry
compressTree input =
  case tChildren input of
    [] -> input
    -- TODO: match a 1-element list and merge
    [child] -> compressTree (mergeEntries input child)
    otherwise -> input {
      tChildren = map compressTree (tChildren input) }


loadRepoTree :: (MonadGit r m) => Commit r -> m DocTreeEntry
loadRepoTree commit = do
  -- This is a list of (TreeFilePath, OrgDoc).  Pull all the paths out
  -- and construct a tree for the ancestor directory structure.
  contents <- getRecursiveContents ("/", CommitEntry (commitOid commit))
  let oid = commitOid commit
      sha = renderOid $ unTagged oid
      makePath (path, doc) = (T.split (=='/')$ TE.decodeUtf8 path, doc)
        -- (T.split (=='/') path, doc)
  -- get SHA of commit
  let root = DocTreeEntry sha (DocTreePath sha [] Nothing) [] [] [] TKDir
      finalTree = foldl' updateTree root $ map makePath contents
      -- err: contents is [(TreeFilePath, OM.OrgDoc)], not [(Text,...)]
  return $ compressTree finalTree
