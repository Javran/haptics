module Trace.Haptics.Tix where

import Data.Int
import Data.Word
import Data.Char
import Data.Attoparsec.Text
import qualified Data.Text as T

data Tix = Tix [TixModule]

type Hash = Word32

data TixModule = TixModule
  { tixModuleName :: T.Text
  , tixModuleHash :: Hash
  , tixModuleListLen :: Int
  , tixModuleTixs :: [Int64]
  }

parseConid :: Parser T.Text
parseConid = do
    c <- satisfy isUpper
    cs <- takeWhile (\c -> isLower c || isUpper c || isDigit c || c == '\'')
    pure (T.cons c cs)
