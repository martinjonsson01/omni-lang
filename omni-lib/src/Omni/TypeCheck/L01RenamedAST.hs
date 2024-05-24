{-# LANGUAGE QuasiQuotes #-}

module Omni.TypeCheck.L01RenamedAST where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Language.Nanopass (deflang)
import Omni.Locations
import Omni.Name (ModuleName, ToGenericName (..))
import Omni.Name qualified as Name
import Omni.TypeCheck.L00AST qualified as L0
import Prettyprinter

-- | A namestore which keeps track of which names are used in which scopes.
data Names = Names
  { getScopes :: NonEmpty Scope
  , usedNames :: Map Name Text
  }
 deriving (Eq, Show)

emptyNames :: Names
emptyNames = Names (NE.singleton Map.empty) Map.empty

-- | Mapping parser names in the current scope to their (module-)globally unique names.
type Scope = Map Text Name

[deflang|
(L1RenamedAST from L0:L0AST
  (* Module
    (- Module)
    (+ Module Loc ModuleName Names (* TopDef))
  )

  (* GenericName
    (- GenName)
    (+ GenName Loc Name:Ident)
  )
)
|]

deriving instance Eq GenericName
deriving instance Eq Name
deriving instance Eq TypeName
deriving instance Eq ConstructorName
deriving instance Eq TypeVarName
deriving instance Eq EffectVarName
deriving instance Eq InfixOpName

deriving instance Ord GenericName
deriving instance Ord Name
deriving instance Ord TypeName
deriving instance Ord ConstructorName
deriving instance Ord TypeVarName
deriving instance Ord EffectVarName
deriving instance Ord InfixOpName

deriving instance Show GenericName
deriving instance Show Name
deriving instance Show TypeName
deriving instance Show ConstructorName
deriving instance Show TypeVarName
deriving instance Show EffectVarName
deriving instance Show InfixOpName

deriving instance Show ValueType
deriving instance Show AbilityType
deriving instance Show ComputationType
deriving instance Show PegType
deriving instance Show PortType
deriving instance Show AdjustmentType
deriving instance Show InterfaceType
deriving instance Show AbilityInitial
deriving instance Show TypeArgument
deriving instance Show TypeVariable

deriving instance Show Module
deriving instance Show TopDef
deriving instance Show DataDef
deriving instance Show InterfaceDef
deriving instance Show FnDef
deriving instance Show FnSig
deriving instance Show CommandSig
deriving instance Show NamedParam
deriving instance Show NamedPort
deriving instance Show Constructor
deriving instance Show Term
deriving instance Show Binding
deriving instance Show ComputationTerm
deriving instance Show ComputationPattern
deriving instance Show ValuePattern

instance ToGenericName GenericName where
  toGeneric (GenName loc (Name.Ident text)) = Name.GenName loc text

instance ToGenericName Name where
  toGeneric (Name generic) = toGeneric generic
instance ToGenericName TypeName where
  toGeneric (TypeName generic) = toGeneric generic
instance ToGenericName ConstructorName where
  toGeneric (ConstructorName generic) = toGeneric generic
instance ToGenericName TypeVarName where
  toGeneric (TypeVarName generic) = toGeneric generic
instance ToGenericName EffectVarName where
  toGeneric (EffectVarName generic) = toGeneric generic

instance Pretty Term where
  pretty = pretty . show
instance Pretty ComputationTerm where
  pretty = pretty . show
instance Pretty ComputationPattern where
  pretty = pretty . show
instance Pretty ValuePattern where
  pretty = pretty . show
