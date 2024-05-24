module Omni.TypeCheck.L00ParseAST (convertParsed) where

import Omni.Abs qualified as Parsed
import Omni.Imports
import Omni.Locations
import Omni.Name qualified as Name
import Omni.TypeCheck.L00AST

{- | Converts the given AST into its corresponding internal representation.

This is only necessary because the BNFC-generated AST and the
nanopass-generated AST are not automatically compatible.
-}
convertParsed :: FilePath -> Parsed.Module -> CompileM env Module
convertParsed file = \case
  Parsed.Module loc' (Parsed.UpperIdent name) topDefs ->
    let loc = toLoc loc'
        moduleName = Name.ModuleName name
     in return $
          Module
            loc
            moduleName
            (map convertTopDef topDefs)
 where
  toLoc = convertPos file

  toText :: Parsed.Name -> (Parsed.BNFC'Position, Text)
  toText = \case
    Parsed.UpperName loc (Parsed.UpperIdent text) -> (loc, text)
    Parsed.LowerName loc (Parsed.LowerIdent text) -> (loc, text)

  convertName :: Parsed.Name -> Name
  convertName (toText -> (loc, name)) = Name $ GenName (toLoc loc) name

  convertEffectVarName :: Parsed.EffectVarName -> EffectVarName
  convertEffectVarName (Parsed.EffectVarName loc (Parsed.LowerIdent name)) =
    EffectVarName $
      GenName
        (toLoc loc)
        name

  convertTypeName :: Parsed.TypeName -> TypeName
  convertTypeName (Parsed.TypeName loc (Parsed.UpperIdent name)) =
    TypeName $
      GenName
        (toLoc loc)
        name

  convertTypeVarName :: Parsed.TypeVarName -> TypeVarName
  convertTypeVarName (Parsed.TypeVarName loc (Parsed.LowerIdent name)) =
    TypeVarName $
      GenName
        (toLoc loc)
        name

  convertConstructorName :: Parsed.ConstructorName -> ConstructorName
  convertConstructorName (Parsed.ConstructorName loc (Parsed.UpperIdent name)) =
    ConstructorName $
      GenName
        (toLoc loc)
        name

  convertInfixOpName :: Parsed.InfixOpName -> InfixOpName
  convertInfixOpName (Parsed.InfixOpName loc (Parsed.InfixOpIdent name)) =
    InfixOpName $ GenName (toLoc loc) name

  convertFnName :: Parsed.FnName -> Name
  convertFnName (Parsed.InfixFnName loc (Parsed.InfixOpIdent name)) = Name $ GenName (toLoc loc) name
  convertFnName (Parsed.OrdinaryFnName _ name) = convertName name

  convertTopDef :: Parsed.TopDef -> TopDef
  convertTopDef = \case
    Parsed.TopFnDef _ fnDef -> TopFnDef (convertFnDef fnDef)
    Parsed.TopDataDef _ dataDef -> TopDataDef (convertDataDef dataDef)
    Parsed.TopInterfaceDef _ interfaceDef -> TopInterfaceDef (convertInterfaceDef interfaceDef)

  convertFnDef :: Parsed.FnDef -> FnDef
  convertFnDef (Parsed.FnDef loc sig body) =
    FnDef
      (toLoc loc)
      (convertFnSig sig)
      (convertTerm body)

  convertTerm :: Parsed.Term -> Term
  convertTerm = \case
    Parsed.EIdent loc name -> EIdent (toLoc loc) (convertName name)
    Parsed.EIntLit loc i -> EIntLit (toLoc loc) i
    Parsed.EUnit loc -> EUnit (toLoc loc)
    Parsed.EApplication loc f args ->
      EApplication
        (toLoc loc)
        (convertTerm f)
        (map convertTerm args)
    Parsed.EInfixOp loc t1 op t2 ->
      EInfixOp
        (toLoc loc)
        (convertTerm t1)
        (convertInfixOpName op)
        (convertTerm t2)
    Parsed.EThunk loc comps ->
      EThunk
        (toLoc loc)
        (map convertComputationTerm comps)
    Parsed.EConLet loc bindings body ->
      EConLet
        (toLoc loc)
        (map convertBinding bindings)
        (convertTerm body)

  convertComputationTerm :: Parsed.ComputationTerm -> ComputationTerm
  convertComputationTerm (Parsed.EComputation loc compPats body) =
    EComputation
      (toLoc loc)
      (map convertComputationPattern compPats)
      (convertTerm body)

  convertComputationPattern :: Parsed.ComputationPattern -> ComputationPattern
  convertComputationPattern = \case
    Parsed.CompPatValue loc valPat ->
      CompPatValue
        (toLoc loc)
        (convertValuePattern valPat)
    Parsed.CompPatRequest loc name valPatList contVar ->
      CompPatRequest
        (toLoc loc)
        (convertName name)
        (convertValuePatternList valPatList)
        (convertName contVar)
    Parsed.CompPatCatchAll loc varName ->
      CompPatCatchAll
        (toLoc loc)
        (convertName varName)

  convertValuePatternList :: Parsed.ValuePatternList -> [ValuePattern]
  convertValuePatternList = \case
    Parsed.NoValuePatterns _ -> []
    Parsed.SomeValuePatterns _ valPats -> map convertValuePattern valPats

  convertValuePattern :: Parsed.ValuePattern -> ValuePattern
  convertValuePattern (Parsed.ValPat loc name valPatList) =
    ValPat
      (toLoc loc)
      (convertName name)
      (convertValuePatternList valPatList)

  convertBinding :: Parsed.Binding -> Binding
  convertBinding (Parsed.BindAnnotated loc name valTyp val) =
    BindAnnotated
      (toLoc loc)
      (convertName name)
      (convertValueType valTyp)
      (convertTerm val)
  convertBinding (Parsed.Bind loc name val) =
    Bind (toLoc loc) (convertName name) (convertTerm val)

  convertFnSig :: Parsed.FnSig -> FnSig
  convertFnSig (Parsed.FnSig loc name ports returnTyp) =
    FnSig
      (toLoc loc)
      (convertFnName name)
      (map convertPort ports)
      (convertPegType returnTyp)

  convertPort :: Parsed.NamedPort -> NamedPort
  convertPort (Parsed.NamedPort loc name typ) =
    NamedPort
      (toLoc loc)
      (convertName name)
      (convertPortType typ)

  convertPortType :: Parsed.PortType -> PortType
  convertPortType = \case
    Parsed.TPortNone loc valTyp -> TPortNone (toLoc loc) (convertValueType valTyp)
    Parsed.TPortSome loc adjTyp valTyp ->
      TPortSome
        (toLoc loc)
        (convertAdjustmentType adjTyp)
        (convertValueType valTyp)

  convertAdjustmentType :: Parsed.AdjustmentType -> AdjustmentType
  convertAdjustmentType (Parsed.TAdjustment loc interfaces) =
    TAdjustment
      (toLoc loc)
      (map convertInterfaceType interfaces)

  convertInterfaceType :: Parsed.InterfaceType -> InterfaceType
  convertInterfaceType (Parsed.TInterface loc name typeArgList) =
    TInterface
      (toLoc loc)
      (convertTypeName name)
      (convertTypeArgList typeArgList)

  convertTypeArgList :: Parsed.TypeArgList -> [TypeArgument]
  convertTypeArgList = \case
    Parsed.NoTypeArgs _ -> []
    Parsed.SomeTypeArgs _ typArgs -> map convertTypeArg typArgs

  convertTypeArg :: Parsed.TypeArgument -> TypeArgument
  convertTypeArg = \case
    Parsed.TArgValue loc valTyp -> TArgValue (toLoc loc) (convertValueType valTyp)
    Parsed.TArgAbility loc abilTyp -> TArgAbility (toLoc loc) (convertAbilityType abilTyp)

  convertAbilityType :: Parsed.AbilityType -> AbilityType
  convertAbilityType = \case
    Parsed.TAbilityInterfaces loc intTyps ->
      TAbilityInterfaces
        (toLoc loc)
        (AbilityInitEffectVar generated (EffectVarName (GenName generated "ε")))
        (map convertInterfaceType intTyps)
    Parsed.TAbilityEffectVar loc eVar ->
      TAbilityInterfaces
        (toLoc loc)
        (AbilityInitEffectVar (toLoc loc) (convertEffectVarName eVar))
        []

  convertValueType :: Parsed.ValueType -> ValueType
  convertValueType = \case
    Parsed.TValueData loc typName typArgList ->
      TValueData
        (toLoc loc)
        (convertTypeName typName)
        (convertTypeArgList typArgList)
    Parsed.TValueComputation loc compTyp ->
      TValueComputation
        (toLoc loc)
        (convertComputationType compTyp)
    Parsed.TValueParam loc typVarName ->
      TValueParam
        (toLoc loc)
        (convertTypeVarName typVarName)
    Parsed.TUnit loc -> TUnit (toLoc loc)
    Parsed.TInt loc -> TInt (toLoc loc)

  convertComputationType :: Parsed.ComputationType -> ComputationType
  convertComputationType (Parsed.TComputation loc portTyps pegTyp) =
    TComputation
      (toLoc loc)
      (map convertPortType portTyps)
      (convertPegType pegTyp)

  convertPegType :: Parsed.PegType -> PegType
  convertPegType = \case
    Parsed.TPegNone loc valTyp -> TPegNone (toLoc loc) (convertValueType valTyp)
    Parsed.TPegSome loc abilTyp valTyp ->
      TPegSome
        (toLoc loc)
        (convertAbilityType abilTyp)
        (convertValueType valTyp)

  convertDataDef :: Parsed.DataDef -> DataDef
  convertDataDef (Parsed.DataDef loc typName typVarList constructorList) =
    DataDef
      (toLoc loc)
      (convertTypeName typName)
      (convertTypeVarList typVarList)
      (convertConstructorList constructorList)

  convertTypeVarList :: Parsed.TypeVarList -> [TypeVariable]
  convertTypeVarList = \case
    Parsed.NoTypeVars _ -> []
    Parsed.SomeTypeVars _ typVars -> map convertTypeVariable typVars

  convertTypeVariable :: Parsed.TypeVariable -> TypeVariable
  convertTypeVariable = \case
    Parsed.TVar loc name -> TVar (toLoc loc) (convertTypeVarName name)
    Parsed.TVarEffect loc name -> TVarEffect (toLoc loc) (convertEffectVarName name)

  convertConstructorList :: Parsed.ConstructorList -> [Constructor]
  convertConstructorList = \case
    Parsed.NoConstructors _ -> []
    Parsed.SomeConstructors _ constructors -> map convertConstructor constructors

  convertConstructor :: Parsed.Constructor -> Constructor
  convertConstructor (Parsed.Constructor loc name valTypList) =
    Constructor
      (toLoc loc)
      (convertConstructorName name)
      (convertValueTypeList valTypList)

  convertValueTypeList :: Parsed.ValueTypeList -> [ValueType]
  convertValueTypeList = \case
    Parsed.NoValueTypes _ -> []
    Parsed.SomeValueTypes _ valTyps -> map convertValueType valTyps

  convertInterfaceDef :: Parsed.InterfaceDef -> InterfaceDef
  convertInterfaceDef (Parsed.InterfaceDef loc typName typVarList commandSigList) =
    InterfaceDef
      (toLoc loc)
      (convertTypeName typName)
      (convertTypeVarList typVarList)
      (convertCommandSigList commandSigList)

  convertCommandSigList :: Parsed.CommandSigList -> [CommandSig]
  convertCommandSigList = \case
    Parsed.NoCommandSigs _ -> []
    Parsed.SomeCommandSigs _ commandSigs -> map convertCommandSig commandSigs

  convertCommandSig :: Parsed.CommandSig -> CommandSig
  convertCommandSig (Parsed.CommandSig loc name params returnTyp) =
    CommandSig
      (toLoc loc)
      (convertName name)
      (map convertParam params)
      (convertValueType returnTyp)

  convertParam :: Parsed.NamedParam -> NamedParam
  convertParam (Parsed.NamedParam loc name valTyp) =
    NamedParam
      (toLoc loc)
      (convertName name)
      (convertValueType valTyp)
