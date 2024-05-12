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
import Omni.TypeCheck.L00AST qualified as L0
import Omni.TypeCheck.L01RenamedAST qualified as L1
import Text.LLVM qualified as LLVM

-- | Different things the compiler can be asked to do.
data Query a where
  SourceDirectories :: Query [Directory]
  BinariesDirectory :: Query Directory
  Files :: Query (HashSet FilePath)
  FileText :: FilePath -> Query Text
  ModuleFile :: Name.ModuleName -> Query (Maybe FilePath)
  ParsedFile :: FilePath -> Query (Maybe L0.Module)
  FileDefinitions :: FilePath -> Query [L0.TopDef]
  RenamedFile :: FilePath -> Query (Maybe L1.Module)
  LLVMModule :: Name.ModuleName -> Query LLVM.Module
  LLVMFiles :: Query [FilePath]
  Executable :: Query (Maybe FilePath)

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
    BinariesDirectory -> h 1 ()
    Files -> h 2 ()
    FileText a -> h 3 a
    ModuleFile a -> h 4 a
    ParsedFile a -> h 5 a
    FileDefinitions a -> h 6 a
    RenamedFile a -> h 7 a
    LLVMModule a -> h 8 a
    LLVMFiles -> h 9 ()
    Executable -> h 10 ()
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