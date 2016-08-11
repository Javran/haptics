module Trace.Haptics.Tix where

import Data.Int
import Data.Word
import qualified Data.ByteString as BS

data Tix = Tix [TixModule]

type Hash = Word32

data TixModule = TixModule
  { tixModuleName :: BS.ByteString
  , tixModuleHash :: Hash
  , tixModuleListLen :: Int
  , tixModuleTixs :: [Int64]
  }
