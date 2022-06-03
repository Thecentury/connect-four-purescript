module OwnPrelude where

import Prelude

import Data.List (List(..), drop)

safeSkip :: forall a . Int -> List a -> List a
safeSkip count _ | count < 0 = Nil
safeSkip 0 list = list
safeSkip toSkip list = drop toSkip list