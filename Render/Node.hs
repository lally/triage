module Render.Node where

import Import
import Data.OrgMode
import Text.Hamlet
--import Text.Blaze.Html

-- returns ([child nodes], [other children])
splitChildren :: Node -> ([NodeChild], [NodeChild])
splitChildren nd =
  let categorizeNd :: NodeChild
                      -> ([NodeChild], [NodeChild])
                      -> ([NodeChild], [NodeChild])
      categorizeNd n@(ChildNode _) (ns, os) = (ns ++ [n], os)
      categorizeNd o (ns, os) = (ns, os ++ [o])
  in Import.foldr categorizeNd ([], []) $ nChildren nd

renderChild :: NodeChild -> t -> Html
renderChild (ChildText line) = [hamlet| #{tlText line} <br />|]
renderChild (ChildDrawer drawer) = [hamlet| #{drName drawer} <br />|]
renderChild (ChildBabel _) = [hamlet| |]
renderChild (ChildTable _) = [hamlet| |]
-- This shouldn't get invoked!
renderChild (ChildNode _) = fail "no ChildNode should be here!"

-- Reminder: MonadGit r m -> r=repo m=outer monad.  The outer monad can
-- be IO, but also some logger monad.
--renderNode :: (Num a, Eq a) => NodeChild -> a -> HtmlUrl
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

renderNode (ChildDrawer _ ) _ = do [hamlet| |]
renderNode (ChildBabel (Babel lines) ) _ = do
  [hamlet|
   <prism-js language="bash">
     $forall (TextLine _ t _) <- lines
       #{t}
   |]

renderNode _ _ = do [hamlet| |]
