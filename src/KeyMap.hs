-- | KeyMap, extending Aeson.KeyMap to support replacing HashMap.
module KeyMap
  ( KeyMap
  , lookupDefault
  , lookup
  , mapMaybe
  , fromList
  , toList
  , singleton

  , Key
  , fromText
  , toText
  , fromString
  , toString
  ) where

import Data.Aeson.Key    (Key, fromText, toText, fromString, toString)
import Data.Aeson.KeyMap (KeyMap, mapMaybe, fromList, toList, singleton)
import Data.Maybe (fromMaybe)

import qualified Data.Aeson.KeyMap as KM

-- | lookupDefault for KeyMap based on lookupDefault from HashMap.
lookupDefault :: v -> Key -> KeyMap v -> v
lookupDefault d k km = fromMaybe d $ KM.lookup k km
