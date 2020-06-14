{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module HashMap where

import qualified Data.HashMap.Strict as HS
import Data.Hashable (Hashable())
import Lens.Micro.Internal

type instance Index (HS.HashMap k v) = k

type instance IxValue (HS.HashMap k v) = v

instance (Eq k, Hashable k) => Ixed (HS.HashMap k v) where
  ix key func = HS.alterF (traverse func) key

instance (Eq k, Hashable k) => At (HS.HashMap k v) where
  at key func = HS.alterF func key
