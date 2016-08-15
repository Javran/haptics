{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Trace.Haptics.Tix where

import Prelude hiding (takeWhile)
import Data.Int
import Data.Word
import Data.Char
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Exception as Exc
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data Tix = Tix (M.Map T.Text TixModule) deriving Show

type Hash = Word32

data TixModule = TixModule
  { tixModuleName :: T.Text
  , tixModuleHash :: Hash
  , tixModuleTixs :: V.Vector Int64
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

between :: Parser open -> Parser close -> Parser a -> Parser a
between op cl m = op *> m <* cl

parseTixModule :: Parser TixModule
parseTixModule = do
    mn <- between
            ("TixModule" >> skipSpace >> "\"")
            ("\"" >> skipSpace)
            parseModuleName
    h <- decimal <* skipSpace
    _ <- decimal <* skipSpace :: Parser Int64
    tixs <- between "[" "]" $
        decimal `sepBy` (char ',' >> skipSpace)
    pure (TixModule mn h (V.fromList tixs))

parseTix :: Parser Tix
parseTix = do
    tms <- between
             ("Tix" >> skipSpace >> "[" >> skipSpace)
             "]"
             (parseTixModule `sepBy` (char ',' >> skipSpace))
    pure (Tix . M.fromList . map (\x -> (tixModuleName x, x)) $ tms)

readTix :: FilePath -> IO (Maybe Tix)
readTix fp = Exc.catch
    (do content <- T.readFile fp
        let result = parseOnly (parseTix <* skipSpace <* endOfInput) content
        pure $ case result of
            Left _ -> Nothing
            Right r -> Just r)
    (\ (_ :: Exc.IOException) -> pure Nothing)

-- TODO: see https://github.com/ghc/ghc/blob/master/utils/hpc/HpcCombine.hs
-- for now our task is to try doing the same but more efficiently,
-- one way would be turning Tix into a Map from (<module name>,<hash>) to tix module data.
mergeTix :: Tix -> Tix -> Tix
mergeTix (Tix tas) (Tix tbs) = Tix (M.unionWith mergeTixModule tas tbs)
  where
    mergeTixModule (TixModule f1 h1 tvs1) (TixModule _ h2 tvs2)
        | h1 /= h2 = error "hash mismatch"
        | V.length tvs1 /= V.length tvs2 = error "tix len mismatch"
        | otherwise = let newTs = V.zipWith (+) tvs1 tvs2 in TixModule f1 h1 newTs

equal :: Eq a => a -> a -> Maybe a
equal v1 v2 = if v1 == v2 then Just v1 else Nothing
