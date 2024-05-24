{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Omni.TypeCheck.L00AST where

import Language.Nanopass (deflang)
import Omni.Locations
import Omni.Name (ModuleName, ToGenericName (..))
import Data.Text (Text)
import qualified Omni.Name as Named

[deflang|
((L0AST)

  (Module
    (Module Loc ModuleName (* TopDef))
  )
  (TopDef
    (TopFnDef FnDef)
    (TopDataDef DataDef)
    (TopInterfaceDef InterfaceDef)
  )

  (FnDef
    (FnDef Loc FnSig Term)
  )
  (FnSig
    (FnSig Loc Name (* NamedPort) PegType)
  )
  (NamedPort
    (NamedPort Loc Name PortType)
  )

  (DataDef 
    (DataDef Loc TypeName (* TypeVariable) (* Constructor))
  )
  (Constructor
    (Constructor Loc ConstructorName (* ValueType))
  )

  (InterfaceDef
    (InterfaceDef Loc TypeName (* TypeVariable) (* CommandSig))
  )
  (CommandSig
    (CommandSig Loc Name (* NamedParam) ValueType)
  )
  (NamedParam
    (NamedParam Loc Name ValueType)
  )

  (ValueType
    (TValueData Loc TypeName (* TypeArgument))
    (TValueComputation Loc ComputationType)
    (TValueParam Loc TypeVarName)
    (TUnit Loc)
    (TInt Loc)
  )
  (ComputationType
    (TComputation Loc (* PortType) PegType)
  )
  (PortType
    (TPortNone Loc ValueType)
    (TPortSome Loc AdjustmentType ValueType)
  )
  (PegType
    (TPegNone Loc ValueType)
    (TPegSome Loc AbilityType ValueType)
  )
  (TypeVariable
    (TVar Loc TypeVarName)
    (TVarEffect Loc EffectVarName)
  )
  (TypeArgument
    (TArgValue Loc ValueType)
    (TArgAbility Loc AbilityType)
  )
  (InterfaceType 
    (TInterface Loc TypeName (* TypeArgument))
  )
  (AbilityType
    (TAbilityInterfaces Loc AbilityInitial (* InterfaceType))
  )
  (AbilityInitial
    (AbilityInitEmpty)
    (AbilityInitEffectVar Loc EffectVarName)
  )
  (AdjustmentType
    (TAdjustment Loc (* InterfaceType))
  )

  (Term
    (EIdent Loc Name)
    (EIntLit Loc Integer)
    (EUnit Loc)
    (EApplication Loc Term (* Term))
    (EInfixOp Loc Term InfixOpName Term)
    (EThunk Loc (* ComputationTerm))
    (EConLet Loc (* Binding) Term)
  )
  (Binding
    (Bind Loc Name Term)
    (BindAnnotated Loc Name ValueType Term)
  )
  (ComputationTerm
    (EComputation Loc (* ComputationPattern) Term)
  )

  (ComputationPattern
    (CompPatValue Loc ValuePattern)
    (CompPatRequest Loc Name (* ValuePattern) Name)
    (CompPatCatchAll Loc Name)
  )
  (ValuePattern
    (ValPat Loc Name (* ValuePattern))
  )

  (GenericName
    (GenName Loc Text)
  )
  (Name 
    (Name GenericName)
  )
  (InfixOpName 
    (InfixOpName GenericName)
  )
  (TypeName 
    (TypeName GenericName)
  )
  (ConstructorName 
    (ConstructorName GenericName)
  )
  (TypeVarName 
    (TypeVarName GenericName)
  )
  (EffectVarName 
    (EffectVarName GenericName)
  )
)
|]

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
  toGeneric (GenName loc text) = Named.GenName loc text
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
instance ToGenericName InfixOpName where 
  toGeneric (InfixOpName generic) = toGeneric generic