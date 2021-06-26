------------------------------------------------------------
-- Andy Gill and Colin Runciman, June 2006
------------------------------------------------------------

-- | Datatypes and file-access routines for the tick data file
-- (@.tix@) used by Hpc.
module Trace.Haptics.Tix
  ( Tix (..)
  , TixModule (..)
  , tixModuleName
  , tixModuleHash
  , tixModuleTixs
  , readTix
  , writeTix
  , getTixFileName
  )
where

import System.FilePath (replaceExtension)
import Trace.Haptics.Utils (Hash, catchIO, readFileUtf8, writeFileUtf8)

-- | 'Tix' is the storage format for our dynamic information about
-- what boxes are ticked.
data Tix = Tix [TixModule]
  deriving (Read, Show, Eq)

data TixModule
  = TixModule
      String --  module name
      Hash --  hash number
      Int --  length of Tix list (allows pre-allocation at parse time).
      [Integer] --  actual ticks
  deriving (Read, Show, Eq)

-- TODO: Turn extractors below into proper 'TixModule' field-labels
tixModuleName :: TixModule -> String
tixModuleName (TixModule nm _ _ _) = nm

tixModuleHash :: TixModule -> Hash
tixModuleHash (TixModule _ h _ _) = h

tixModuleTixs :: TixModule -> [Integer]
tixModuleTixs (TixModule _ _ _ tixs) = tixs

-- We /always/ read and write Tix from the current working directory.

-- | Read a @.tix@ File.
readTix
  :: String
  -> IO (Maybe Tix)
readTix tixFilename =
  catchIO
    (fmap (Just . read) $ readFileUtf8 tixFilename)
    (const $ return Nothing)

-- | Write a @.tix@ File.
writeTix
  :: String
  -> Tix
  -> IO ()
writeTix name tix = writeFileUtf8 name (show tix)

-- | 'getTixFullName' takes a binary or @.tix@-file name,
-- and normalizes it into a @.tix@-file name.
getTixFileName :: String -> String
getTixFileName str = replaceExtension str "tix"
