{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Prelude
import Fay.Yesod
import Data.Data
import Data.Text
#ifdef FAY
import FFI
#else
import Fay
import Fay.FFI
#endif
{-
data DrawableNode = DrawableNode { dnTitle ∷ String
                                 , dnTags ∷ [String]
                                 , dnState ∷ String
                                 , dnDate ∷ String }
                    deriving (Read, Typeable, Data)

data NodeTree = NodeLeaf DrawableNode
              | NodeParent DrawableNode [NodeTree]
                deriving (Read, Typeable, Data)
-}
data Command = LookupRef Text (Returns Text)
--             | LoadGraph Text (Returns NodeTree)
    deriving (Read, Typeable, Data)
