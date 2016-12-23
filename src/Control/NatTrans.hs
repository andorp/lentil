{-# LANGUAGE RankNTypes #-}
module Control.NatTrans where

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Maybe as Maybe


newtype NatTrans f g = NT { runNT :: forall a . f a -> g a }

instance Category NatTrans where
  id                = NT id
  (NT bc) . (NT ab) = NT (bc . ab)

listToMaybe :: NatTrans [] Maybe
listToMaybe = NT Maybe.listToMaybe

maybeToList :: NatTrans Maybe []
maybeToList = NT Maybe.maybeToList

