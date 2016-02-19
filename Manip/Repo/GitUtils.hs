module Manip.Repo.GitUtils where
import Git
import Data.Text
import Import as P
-- | git_reference_dwim(), essentially.  See if the text's a SHA, or
-- if it's a branch:sha, or a symbolic name like HEAD (default branch
-- can be given if not master).
resolveReferenceDwim :: (MonadGit r m) => Text -> (Maybe Text) -> m (Maybe (RefTarget r))
resolveReferenceDwim reference defaultBranch = undefined
  
{-  trySha <- resolveReverence reference
  if isJust trySha
    then do return (Just (RefOid $ fromJust trySha))
    else do
      let refName = if hasColon reference
                    then reference
                    else defaultBranch ++ ":" ++ refernce
      ref <- resolveReference 
  return Nothing-}
