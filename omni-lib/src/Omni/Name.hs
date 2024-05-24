{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Omni.Name (
  -- * A generic name that can be associated with any construct
  GenericName (..),
  ToGenericName (..),

  -- * Different kinds of names for different constructs
  ModuleName (..),
  Ident (..),
) where

import Data.Hashable
import Data.String
import Data.Text
import Omni.Abs qualified as Parsed
import Omni.Locations (Loc)
import Prettyprinter (Pretty (pretty))

data GenericName = GenName Loc Text
  deriving (Show)

instance Eq GenericName where
  GenName _ a == GenName _ b = a == b

instance Ord GenericName where
  GenName _ a <= GenName _ b = a <= b

instance {-# OVERLAPPABLE #-} (Show n, ToGenericName n) => Pretty n where
  pretty (toGeneric -> gen) = pretty gen

class ToGenericName a where
  toGeneric :: a -> GenericName

instance ToGenericName GenericName where
  toGeneric = id

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