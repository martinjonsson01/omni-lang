{-# LANGUAGE QuasiQuotes #-}

module Omni.TypeCheck.L02Elaborated where

import Data.Foldable
import Data.Set (Set)
import Language.Nanopass (deflang)
import Omni.Abs qualified as Parsed
import Omni.Locations
import Omni.Name (toGeneric)
import Omni.Name qualified as Name
import Omni.TypeCheck.L01RenamedAST qualified as L1
import Prettyprinter

[deflang|
(L2Elaborated from L1:L1RenamedAST

  (* FnDef
    (- FnDef)
    (+ FnDef Loc FnSig TypedTerm)
  )

  (+ PolyType
    (TPoly Loc (Set TypeVarName) ValueType)
  )

  (* ValueType
    (+ TPoisoned)
  )

  (* PegType
    (- TPegNone)
    
    (- TPegSome)
    (+ TPeg Loc AbilityType ValueType)
  )

  (+ TypedTerm
    (Typed ValueType Term)
  )
  (* Term
    (- EApplication)
    (+ EApplication Loc TypedTerm (* TypedTerm))

    (- EInfixOp)
    (+ EInfixOp Loc TypedTerm Parsed:InfixOpIdent TypedTerm)

    (- EThunk)
    (+ EThunk Loc (* TypedComputationTerm))

    (- EConLet)
    (+ EConLet Loc (* Binding) TypedTerm)
  )
  (* Binding
    (- Bind)
    (- BindAnnotated)
    (+ BindAnnotated Loc Name TypedTerm)
  )

  (+ TypedComputationTerm
    (TypedComp ComputationType ComputationTerm)
  )
  (* ComputationTerm
    (- EComputation)
    (+ EComputation Loc (* ComputationPattern) TypedTerm)
  )
)
|]

instance Ord GenericName where
  GenName _ x <= GenName _ y = x <= y

deriving instance Ord Name
deriving instance Ord TypeName
deriving instance Ord ConstructorName
deriving instance Ord TypeVarName
deriving instance Ord EffectVarName
deriving instance Ord InfixOpName
deriving instance Ord TypeVariable

instance Eq GenericName where
  GenName _ x == GenName _ y = x == y

deriving instance Eq Name
deriving instance Eq ConstructorName
deriving instance Eq AbilityType
deriving instance Eq AbilityInitial
deriving instance Eq EffectVarName
deriving instance Eq InterfaceType
deriving instance Eq TypeName
deriving instance Eq TypeArgument
deriving instance Eq ValueType
deriving instance Eq ComputationType
deriving instance Eq PortType
deriving instance Eq AdjustmentType
deriving instance Eq PegType
deriving instance Eq TypeVarName
deriving instance Eq InfixOpName
deriving instance Eq TypeVariable

deriving instance Show GenericName
deriving instance Show Name
deriving instance Show TypeName
deriving instance Show TypeVarName
deriving instance Show EffectVarName
deriving instance Show InfixOpName

deriving instance Show ValueType
deriving instance Show AbilityType
deriving instance Show PolyType
deriving instance Show ComputationType
deriving instance Show PegType
deriving instance Show PortType
deriving instance Show AdjustmentType
deriving instance Show InterfaceType
deriving instance Show AbilityInitial
deriving instance Show TypeArgument

deriving instance Show Term
deriving instance Show TypedComputationTerm
deriving instance Show TypedTerm
deriving instance Show Binding
deriving instance Show ComputationTerm
deriving instance Show ComputationPattern
deriving instance Show ValuePattern

instance Pretty ValueType where
  pretty = \case
    TValueData _ typName typArgs -> sep $ pretty typName : map pretty typArgs
    TValueComputation _ compT -> pretty compT
    TValueParam _ typVarName -> pretty typVarName
    TUnit _ -> pretty ("Unit" :: String)
    TInt _ -> pretty ("Int" :: String)
    TPoisoned -> pretty ("POISONED" :: String)
instance Pretty Term where
  pretty = pretty . show
instance Pretty AbilityType where
  pretty (TAbilityInterfaces _ inital interfaces) =
    enclose "<" ">" (sep $ pretty inital : map pretty interfaces)
instance Pretty PortType where
  pretty = pretty . show
instance Pretty PolyType where
  pretty (TPoly _ typVars typ) =
    sep $
      "forall "
        : punctuate comma (map pretty (toList typVars))
        ++ [".", pretty typ]
instance Pretty ComputationType where
  pretty = pretty . show
instance Pretty TypeArgument where
  pretty = pretty . show
instance Pretty AbilityInitial where
  pretty = pretty . show
instance Pretty InterfaceType where
  pretty = pretty . show

instance Name.ToGenericName GenericName where
  toGeneric (GenName loc (Name.Ident name)) = Name.GenName loc name
instance Name.ToGenericName TypeName where
  toGeneric (TypeName gen) = toGeneric gen
instance Name.ToGenericName Name where
  toGeneric (Name gen) = toGeneric gen
instance Name.ToGenericName ConstructorName where
  toGeneric (ConstructorName gen) = toGeneric gen
instance Name.ToGenericName TypeVarName where
  toGeneric (TypeVarName gen) = toGeneric gen
instance Name.ToGenericName EffectVarName where
  toGeneric (EffectVarName gen) = toGeneric gen

instance Located ValueType where
  getLoc = \case
    TValueData loc _ _ -> loc
    TValueComputation loc _ -> loc
    TValueParam loc _ -> loc
    TUnit loc -> loc
    TInt loc -> loc
    TPoisoned -> generated

instance Located PolyType where
  getLoc (TPoly loc _ _) = loc

instance Located PortType where
  getLoc = \case
    TPortNone loc _ -> loc
    TPortSome loc _ _ -> loc

instance Located GenericName where
  getLoc (GenName loc _) = loc
instance Located Name where
  getLoc (Name generic) = getLoc generic
