{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Trace.Haptics.Tix where

import Prelude hiding (takeWhile)
import Data.Functor
import Data.Int
import Data.Word
import Data.Char
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Exception as Exc
import Debug.Trace

data Tix = Tix [TixModule] deriving Show

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
    "TixModule" >> skipSpace
    void "\""
    mn <- parseModuleName
    "\"" >> skipSpace
    h <- decimal <* skipSpace
    l <- decimal <* skipSpace
    void "["
    tixs <- decimal `sepBy` (char ',' >> skipSpace)
    void "]"
    pure (TixModule mn h l tixs)

parseTix :: Parser Tix
parseTix = do
    "Tix" >> skipSpace
    "[" >> skipSpace
    tms <- parseTixModule `sepBy` (char ',' >> skipSpace)
    void "]"
    pure (Tix tms)

readTix :: FilePath -> IO (Maybe Tix)
readTix fp = Exc.catch
    (do content <- T.readFile fp
        let result = parseOnly (parseTix <* skipSpace <* endOfInput) content
        pure $ case result of
            Left i -> traceShow (show i) Nothing
            Right r -> Just r)
    (\ (_ :: Exc.IOException) -> pure Nothing)
