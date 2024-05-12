{-# OPTIONS_GHC -Wno-orphans #-}

module Omni.Name (
  -- * Different kinds of names for different constructs
  ModuleName (..),
  Ident (..),
) where

import Data.Hashable
import Data.String
import Data.Text
import Omni.Abs qualified as Parsed
import Prettyprinter (Pretty (pretty))

newtype ModuleName = ModuleName Text
  deriving (Eq, Ord, Show, IsString, Hashable)

instance Pretty ModuleName where
  pretty (ModuleName t) = pretty t

newtype Ident = Ident Text
  deriving (Eq, Ord, Show, IsString, Hashable)

instance Pretty Ident where
  pretty (Ident t) = pretty t

instance Pretty Parsed.Name where
  pretty (Parsed.UpperName _ ident) = pretty ident
  pretty (Parsed.LowerName _ ident) = pretty ident

instance Pretty Parsed.UpperIdent where
  pretty (Parsed.UpperIdent t) = pretty t

instance Pretty Parsed.LowerIdent where
  pretty (Parsed.LowerIdent t) = pretty t