{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Omni.Query (Query (..)) where

import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.HashSet
import Data.Hashable
import Data.Some
import Data.Text
import Omni.Config (Directory)
import Omni.Name qualified as Name
import Omni.Abs qualified as Parsed
import qualified Text.LLVM as LLVM

-- | Different things the compiler can be asked to do.
data Query a where
  SourceDirectories :: Query [Directory]
  Files :: Query (HashSet FilePath)
  FileText :: FilePath -> Query Text
  ModuleFile :: Name.Module -> Query (Maybe FilePath)
  ParsedFile :: FilePath -> Query (Maybe Parsed.Module)
  LLVMModule :: Name.Module -> Query LLVM.Module

deriving instance Eq (Query a)

deriving instance Show (Query a)

deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query

instance Hashable (Query a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt =
    defaultHashWithSalt

  hash query = case query of
    SourceDirectories -> h 0 ()
    Files -> h 1 ()
    FileText a -> h 2 a
    ModuleFile a -> h 3 a
    ParsedFile a -> h 4 a
    LLVMModule a -> h 5 a
   where
    -- Hashes the query key with a unique index and its payload.
    {-# INLINE h #-}
    h :: (Hashable b) => Int -> b -> Int
    h tag payload =
      hash tag `hashWithSalt` payload

instance Hashable (Some Query) where
  {-# INLINE hash #-}
  hash (Some query) =
    hash query

  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (Some query) =
    hashWithSalt salt query