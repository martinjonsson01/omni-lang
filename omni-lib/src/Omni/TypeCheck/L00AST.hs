{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Omni.TypeCheck.L00AST where

import Language.Nanopass (deflang)
import Omni.Abs qualified as Parsed
import Omni.Locations
import Omni.Name (ModuleName)
import Data.Text (Text)

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
    (TAbilityInterfaces Loc (* InterfaceType))
    (TAbilityEffectVar Loc EffectVarName)
    (TAbilityEmpty Loc)
  )
  (AdjustmentType
    (TAdjustment Loc (* InterfaceType))
  )

  (Term
    (EIdent Loc Name)
    (EIntLit Loc Integer)
    (EUnit Loc)
    (EApplication Loc Term (* Term))
    (EInfixOp Loc Term Parsed:InfixOpIdent Term)
    (EConSuspendedCom Loc (* ComputationTerm))
    (EConLet Loc (* Binding) Term)
  )
  (Binding
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

class ToGenericName a where 
  toGeneric :: a -> GenericName

instance ToGenericName GenericName where 
  toGeneric generic = generic
instance ToGenericName Name where 
  toGeneric (Name generic) = generic
instance ToGenericName TypeName where 
  toGeneric (TypeName generic) = generic
instance ToGenericName ConstructorName where 
  toGeneric (ConstructorName generic) = generic
instance ToGenericName TypeVarName where 
  toGeneric (TypeVarName generic) = generic
instance ToGenericName EffectVarName where 
  toGeneric (EffectVarName generic) = generic