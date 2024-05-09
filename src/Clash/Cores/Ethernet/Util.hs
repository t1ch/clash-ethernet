{-|
Module      : Clash.Cores.Ethernet.Util
Description : Utility module, only for very small util functions
-}
module Clash.Cores.Ethernet.Util
    ( toMaybe
    ) where

import Clash.Prelude

-- | Wrap a value in a Just if True
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

