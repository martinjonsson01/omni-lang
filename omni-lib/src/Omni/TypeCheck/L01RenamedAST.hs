{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingVia #-}

module Omni.TypeCheck.L01RenamedAST where

import Language.Nanopass (deflang)
import Omni.Locations
import Omni.Name qualified as Name
import Omni.TypeCheck.L00AST qualified as L0

[deflang|
(L1RenamedAST from L0:L0AST
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

deriving instance Ord GenericName
deriving instance Ord Name
deriving instance Ord TypeName
deriving instance Ord ConstructorName
deriving instance Ord TypeVarName
deriving instance Ord EffectVarName

deriving instance Show GenericName
deriving instance Show Name
deriving instance Show TypeName
deriving instance Show ConstructorName
deriving instance Show TypeVarName
deriving instance Show EffectVarName

instance L0.ToGenericName GenericName where 
  toGeneric (GenName loc (Name.Ident text)) = L0.GenName loc text

instance L0.ToGenericName Name where 
  toGeneric (Name generic) = L0.toGeneric generic
instance L0.ToGenericName TypeName where 
  toGeneric (TypeName generic) = L0.toGeneric generic
instance L0.ToGenericName ConstructorName where 
  toGeneric (ConstructorName generic) = L0.toGeneric generic
instance L0.ToGenericName TypeVarName where 
  toGeneric (TypeVarName generic) = L0.toGeneric generic
instance L0.ToGenericName EffectVarName where 
  toGeneric (EffectVarName generic) = L0.toGeneric generic