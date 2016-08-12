{-# LANGUAGE OverloadedStrings #-}
module Trace.Haptics.Tix where

import Prelude hiding (takeWhile)
import Data.Functor
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
  } deriving Show

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

parseTixModule :: Parser TixModule
parseTixModule = do
    void "TixModule"
    skipSpace
    void "\""
    mn <- parseModuleName
    void "\""
    skipSpace
    h <- decimal
    skipSpace
    l <- decimal
    skipSpace
    void "["
    tixs <- decimal `sepBy` (char ',' >> skipSpace)
    void "]"
    pure (TixModule mn h l tixs)
