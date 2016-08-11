{-# LANGUAGE OverloadedStrings #-}
module Trace.Haptics.Tix where

import Prelude hiding (takeWhile)
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

parseModuleName :: Parser T.Text
parseModuleName = T.intercalate "." <$> parseModid
  where
    parseConid :: Parser T.Text
    parseConid = do
        ch <- satisfy isUpper
        cs <- takeWhile (\c -> isLower c || isUpper c || isDigit c || c == '\'')
        pure (T.cons ch cs)

    parseModid :: Parser [T.Text]
    parseModid = parseConid `sepBy1` char '.'
