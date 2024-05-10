{-# OPTIONS_GHC -Wno-orphans #-}

module Omni.Name (
  -- * Different kinds of names for different constructs
  Module (..),
  Ident (..),
) where

import Data.Hashable
import Data.String
import Data.Text
import Omni.Abs qualified as Parsed
import Prettyprinter (Pretty (pretty))

newtype Module = Module Text
  deriving (Eq, Ord, Show, IsString, Hashable)

instance Pretty Module where
  pretty (Module t) = pretty t

newtype Ident = Ident Text
  deriving (Eq, Ord, Show, IsString, Hashable)

instance Pretty Ident where
  pretty (Ident t) = pretty t

instance Pretty Parsed.Name where
  pretty (Parsed.Name _ ident) = pretty ident

instance Pretty Parsed.Ident where
  pretty (Parsed.Ident t) = pretty t