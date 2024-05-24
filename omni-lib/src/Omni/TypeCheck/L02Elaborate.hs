{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans
  -Wno-name-shadowing
  -Wno-unused-top-binds
  -Wno-redundant-constraints #-}

module Omni.TypeCheck.L02Elaborate (elaborate) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Language.Nanopass (defpass)
import Omni.Imports
import Omni.Locations
import Omni.Name (ToGenericName (..))
import Omni.Name qualified as Name
import Omni.TypeCheck.L01RenamedAST qualified as L1
import Omni.TypeCheck.L02Elaborated

-- | Typing context containg information about terms' types.
data Context = Context
  { terms :: Map Name.GenericName ContextEntry
  , ports :: Map Name.GenericName PortType
  , ambientAbility :: AbilityType
  , dataTypes :: Map TypeName DataDef
  , interfaces :: Map TypeName InterfaceDef
  , rigidTypeVars :: Map Name.GenericName TypeVariable
  , typeVarBindings :: Map TypeVariable TypeArgument
  }

getAmbientAbility :: ElaborateM env AbilityType
getAmbientAbility = gets ambientAbility

{- | Executes the given computation inside a context with an ambient ability
which has been extended with the given adjustment.
-}
withAdjustment :: AdjustmentType -> ElaborateM env a -> ElaborateM env a
withAdjustment adjustment m = do
  ambAbil <- getAmbientAbility
  withAmbientAbility (adjustAbility ambAbil adjustment) m

-- | Applies an adjustment to the current ambient ability, without mutating the context.
adjustAmbientAbility :: AdjustmentType -> ElaborateM env AbilityType
adjustAmbientAbility adjustment = adjustAbility <$> getAmbientAbility <*> pure adjustment

-- | Applies an adjustment to the given ability.
adjustAbility :: AbilityType -> AdjustmentType -> AbilityType
adjustAbility abil (TAdjustment _ []) = abil
adjustAbility (TAbilityInterfaces loc init interfaces) (TAdjustment _ newInterfaces) =
  TAbilityInterfaces loc init (newInterfaces ++ interfaces) -- todo: not sure the order of interfaces here is correct... double check

-- | Executes the given computation inside a context with the given ambient ability.
withAmbientAbility :: AbilityType -> ElaborateM env a -> ElaborateM env a
withAmbientAbility abil m = do
  ambAbil <- getAmbientAbility

  modify $ \s -> s{ambientAbility = abil}
  value <- m
  modify $ \s -> s{ambientAbility = ambAbil}

  return value

-- | Instantiates a (module-)globally unique type variable (unique until unified).
instantiateTypeVar :: Loc -> TypeVarName -> ElaborateM env ()
instantiateTypeVar loc name = do
  ctxTypeVars <- gets rigidTypeVars
  modify $ \s -> s{rigidTypeVars = Map.insert (toGeneric name) (TVar loc name) ctxTypeVars}

-- | Instantiates a (module-)globally unique effect variable (unique until unified).
instantiateEffectVar :: Loc -> EffectVarName -> ElaborateM env ()
instantiateEffectVar loc name = do
  ctxTypeVars <- gets rigidTypeVars
  modify $ \s ->
    s
      { rigidTypeVars = Map.insert (toGeneric name) (TVarEffect loc name) ctxTypeVars
      }

-- | Registers the (mono-)type of a given variable to the context.
registerMonoVar :: (ToGenericName n) => n -> ValueType -> ElaborateM env ()
registerMonoVar name typ = do
  ctxTerms <- gets terms
  let entry = Mono typ
  let genName = toGeneric name
  modify $ \s -> s{terms = Map.insert genName entry ctxTerms}

-- | Registers the (poly-)type of a given variable to the context.
registerPolyVar :: (ToGenericName n) => Loc -> n -> ValueType -> ElaborateM env ()
registerPolyVar loc name typ = do
  ctxTerms <- gets terms
  let freeVars = findFreeVars typ
      entry = Poly (TPoly loc freeVars typ)
  let genName = toGeneric name
  modify $ \s -> s{terms = Map.insert genName entry ctxTerms}
 where
  -- todo: take into account bound type variables in current context
  findFreeVars :: ValueType -> Set TypeVarName
  findFreeVars t = execWriter (go t)
   where
    go :: ValueType -> Writer (Set TypeVarName) ()
    go = \case
      TValueData{} -> return ()
      TValueComputation _ (TComputation _ ports peg) -> mapM_ goPortT ports >> goPegT peg
      TValueParam _ typVarName -> tell $ Set.singleton typVarName
      TUnit _ -> return ()
      TInt _ -> return ()
      TPoisoned -> return ()
    goPegT :: PegType -> Writer (Set TypeVarName) ()
    goPegT (TPeg _ _ valT) = go valT
    goPortT :: PortType -> Writer (Set TypeVarName) ()
    goPortT = \case
      TPortNone _ valT -> go valT
      TPortSome _ _ valT -> go valT

-- | Registers the type of a given port to the context.
registerPortVar :: (ToGenericName n) => n -> PortType -> ElaborateM env ()
registerPortVar name typ = do
  ctxPorts <- gets ports
  let genName = toGeneric name
  modify $ \s -> s{ports = Map.insert genName typ ctxPorts}

-- | Registers a new data type to the context.
registerDataType :: DataDef -> ElaborateM env ()
registerDataType dataDef@(DataDef _ name _ _) = do
  ctxDataTypes <- gets dataTypes
  modify $ \s -> s{dataTypes = Map.insert name dataDef ctxDataTypes}

-- | Registers a new ability interface to the context.
registerInterface :: InterfaceDef -> ElaborateM env ()
registerInterface intDef@(InterfaceDef _ name _ _) = do
  ctxInterfaces <- gets interfaces
  modify $ \s -> s{interfaces = Map.insert name intDef ctxInterfaces}

-- | Looks up a type variable, failing the computation if it doesn't exist.
lookupTypeVar :: (ToGenericName n) => n -> ElaborateM env TypeVariable
lookupTypeVar (toGeneric -> name) = do
  ctxTypeVars <- gets rigidTypeVars
  maybe mzero return (Map.lookup name ctxTypeVars)

-- | Looks up the bound type of a type variable.
lookupTypeVarBinding :: TypeVariable -> ElaborateM env (Maybe TypeArgument)
lookupTypeVarBinding typVar = do
  ctxBindings <- gets typeVarBindings
  return $ Map.lookup typVar ctxBindings

{- | Looks up the type of the given name in the current environment, failing
the computation if none is found. Any type or effect variables are instantiated.
-}
lookupVar :: (ToGenericName n) => n -> ElaborateM env ValueType
lookupVar (toGeneric -> name) = do
  ctxTerms <- gets terms
  case Map.lookup name ctxTerms of
    Nothing -> mzero
    Just (Mono t) -> return t
    Just (Poly (TPoly _ freeTypVars t)) -> instantiateTypeVars freeTypVars t

{- | Looks up the value type of the given port in the current environment, failing
the computation if none is found.
-}
lookupPortValueType :: (ToGenericName n) => n -> ElaborateM env ValueType
lookupPortValueType (toGeneric -> name) = do
  ctxPorts <- gets ports
  case Map.lookup name ctxPorts of
    Nothing -> mzero
    Just (TPortNone _ valT) -> return valT
    Just (TPortSome _ _ valT) -> return valT

instantiateTypeVars :: Set TypeVarName -> ValueType -> ElaborateM env ValueType
instantiateTypeVars freeTypVars t = case t of
  TValueComputation loc compT -> TValueComputation loc <$> instantiateCompTypeVars freeTypVars compT
  TValueParam loc typVarName ->
    when
      (typVarName `Set.member` freeTypVars)
      (instantiateTypeVar loc typVarName)
      >> return t
  TValueData loc typName typArgs ->
    TValueData loc typName <$> mapM (instantiateTypeArgTypeVars freeTypVars) typArgs
  TUnit{} -> return t
  TInt{} -> return t
  TPoisoned -> return t

instantiateTypeArgTypeVars :: Set TypeVarName -> TypeArgument -> ElaborateM env TypeArgument
instantiateTypeArgTypeVars freeTypVars = \case
  TArgValue loc valT -> TArgValue loc <$> instantiateTypeVars freeTypVars valT
  TArgAbility loc abilT -> TArgAbility loc <$> instantiateAbilityTypeVars freeTypVars abilT

instantiateAbilityTypeVars :: Set TypeVarName -> AbilityType -> ElaborateM env AbilityType
instantiateAbilityTypeVars freeTypVars = \case
  TAbilityInterfaces loc AbilityInitEmpty interfaces ->
    TAbilityInterfaces loc AbilityInitEmpty
      <$> mapM (instantiateInterfaceTypeVars freeTypVars) interfaces
  TAbilityInterfaces loc (AbilityInitEffectVar varLoc effectVarName) interfaces ->
    instantiateEffectVar varLoc effectVarName
      >> TAbilityInterfaces loc (AbilityInitEffectVar varLoc effectVarName)
        <$> mapM (instantiateInterfaceTypeVars freeTypVars) interfaces

instantiateCompTypeVars :: Set TypeVarName -> ComputationType -> ElaborateM env ComputationType
instantiateCompTypeVars freeTypVars (TComputation loc ports peg) =
  TComputation loc
    <$> mapM (instantiatePortTypeVars freeTypVars) ports
    <*> instantiatePegTypeVars freeTypVars peg

instantiatePortTypeVars :: Set TypeVarName -> PortType -> ElaborateM env PortType
instantiatePortTypeVars freeTypVars = \case
  TPortNone loc valT -> TPortNone loc <$> instantiateTypeVars freeTypVars valT
  TPortSome loc adjT valT ->
    TPortSome loc
      <$> instantiateAdjustmentTypeVars freeTypVars adjT
      <*> instantiateTypeVars freeTypVars valT

instantiateAdjustmentTypeVars :: Set TypeVarName -> AdjustmentType -> ElaborateM env AdjustmentType
instantiateAdjustmentTypeVars freeTypVars (TAdjustment loc interfaces) =
  TAdjustment loc
    <$> mapM (instantiateInterfaceTypeVars freeTypVars) interfaces

instantiateInterfaceTypeVars :: Set TypeVarName -> InterfaceType -> ElaborateM env InterfaceType
instantiateInterfaceTypeVars freeTypVars (TInterface loc typName typArgs) =
  TInterface loc typName
    <$> mapM (instantiateTypeArgTypeVars freeTypVars) typArgs

instantiatePegTypeVars :: Set TypeVarName -> PegType -> ElaborateM env PegType
instantiatePegTypeVars freeTypVars (TPeg loc abilT valT) =
  TPeg loc
    <$> instantiateAbilityTypeVars freeTypVars abilT
    <*> instantiateTypeVars freeTypVars valT

{- | Looks up the definition of the interface in the current environment, failing
the computation if none is found.
-}
lookupInterface :: TypeName -> ElaborateM env InterfaceDef
lookupInterface name = do
  ctxInterfaces <- gets interfaces
  maybe mzero return (Map.lookup name ctxInterfaces)

{- | Looks up the type of the given command in the ambient ability, failing
the computation if none is found.
-}
lookupCommand :: (ToGenericName n) => n -> ElaborateM env CommandSig
lookupCommand name = getAmbientAbility >>= \ability -> lookupCommandInAbility ability name

{- | Looks up the type of the given command in the given environment, failing
the computation if none is found.
-}
lookupCommandInAbility :: (ToGenericName n) => AbilityType -> n -> ElaborateM env CommandSig
lookupCommandInAbility (TAbilityInterfaces _ _ ambInterfaces) (toGeneric -> name) = do
  msum $ flip map ambInterfaces \(TInterface _ interfaceName _) -> do
    InterfaceDef _ _ _ sigs <- lookupInterface interfaceName
    msum $ flip map sigs \sig@(CommandSig _ commandName _ _) ->
      if toGeneric commandName == name then return sig else mzero

{- | Looks up the constructor with the given name (and the type it belongs to),
failing the computation if none is found.
-}
lookupConstructor :: (ToGenericName n) => n -> ElaborateM env (Constructor, ValueType)
lookupConstructor (toGeneric -> name) = do
  ctxDataDefs <- gets (Map.elems . dataTypes)
  msum $ flip map ctxDataDefs \def@(DataDef _ _ _ constructors) ->
    msum $ flip map constructors \ctr@(Constructor _ ctrName _) ->
      if toGeneric ctrName == name then return (ctr, toDataType def) else mzero
 where
  toDataType :: DataDef -> ValueType
  toDataType (DataDef loc typName [] _) = TValueData loc typName []
  toDataType _ = todo "implement support for type parameters in data types"

-- | Converts the given command signature to its corresponding computation type.
commandSigToComputationType :: CommandSig -> ComputationType
commandSigToComputationType (CommandSig loc _ params returnT) =
  let returnTLoc = getLoc returnT
   in TComputation
        loc
        (map (TPortNone loc . getNamedParamType) params)
        (TPeg returnTLoc (implicitAbilityVar returnTLoc) returnT)

-- | Checks that two abilities are equal, unifying them if need be.
abilityEq :: AbilityType -> AbilityType -> ElaborateM env ()
TAbilityInterfaces _ initialA interfacesA
  `abilityEq` TAbilityInterfaces _ initialB interfacesB = 
    initialA `initialEq` initialB >> unless (interfacesA == interfacesB) mzero
   where
    initialEq init1 init2 = case (init1, init2) of
      (AbilityInitEmpty, AbilityInitEmpty) -> return ()
      (AbilityInitEffectVar _ effectVar1, AbilityInitEffectVar _ effectVar2) ->
        tryUnifyTypeVars effectVar1 effectVar2
      (AbilityInitEffectVar loc effectVar, AbilityInitEmpty) -> setClosedAbility loc effectVar
      (AbilityInitEmpty, AbilityInitEffectVar loc effectVar) -> setClosedAbility loc effectVar

    setClosedAbility :: Loc -> EffectVarName -> ElaborateM env ()
    setClosedAbility loc effectVar = do
      typVar <- lookupTypeVar effectVar
      ctxBindings <- gets typeVarBindings
      modify $ \s ->
        s
          { typeVarBindings =
              Map.insert
                typVar
                (TArgAbility loc (closedAbility loc))
                ctxBindings
          }

-- | Checks that two value types are equal, unifying them if need be.
typeEq :: ValueType -> ValueType -> ElaborateM env ()
TPoisoned `typeEq` _ = return ()
_ `typeEq` TPoisoned = return ()
TValueData _ typName1 typArgs1 `typeEq` TValueData _ typName2 typArgs2 =
  unless (typName1 == typName2 && typArgs1 == typArgs2) mzero
TValueComputation _ comp1 `typeEq` TValueComputation _ comp2 =
  comp1 `compTypeEq` comp2
TValueParam _ typVarName1 `typeEq` TValueParam _ typVarName2 =
  tryUnifyTypeVars typVarName1 typVarName2
TValueParam _ typVarName `typeEq` t = tryUnifyTypeVar typVarName t
t `typeEq` TValueParam _ typVarName = tryUnifyTypeVar typVarName t
TUnit _ `typeEq` TUnit _ = return ()
TInt _ `typeEq` TInt _ = return ()
_ `typeEq` _ = mzero

-- | Tries to unify a type variable with a type, failing the computation if it can't.
tryUnifyTypeVar :: (ToGenericName n) => n -> ValueType -> ElaborateM env ()
tryUnifyTypeVar typVarName valT = do
  typVar <- lookupTypeVar typVarName
  mbBoundTyp <- lookupTypeVarBinding typVar
  case mbBoundTyp of
    Nothing -> do
      ctxBindings <- gets typeVarBindings
      modify $ \s ->
        s
          { typeVarBindings = Map.insert typVar (TArgValue (getLoc valT) valT) ctxBindings
          }
    Just (TArgValue _ boundValT) -> boundValT `typeEq` valT
    Just (TArgAbility{}) -> mzero

-- | Tries to unify two type variables, failing the computation if it can't.
tryUnifyTypeVars :: (ToGenericName n1, ToGenericName n2) => n1 -> n2 -> ElaborateM env ()
tryUnifyTypeVars name1 name2 = do
  typVar1 <- lookupTypeVar name1
  typVar2 <- lookupTypeVar name2
  mbBoundTyp1 <- lookupTypeVarBinding typVar1
  mbBoundTyp2 <- lookupTypeVarBinding typVar2
  case (mbBoundTyp1, mbBoundTyp2) of
    (Just (TArgValue _ boundTyp), _) -> tryUnifyTypeVar name2 boundTyp
    (_, Just (TArgValue _ boundTyp)) -> tryUnifyTypeVar name1 boundTyp
    _ -> mzero

{- | Checks that two computation types are equal, unifying them if need be.
todo: propagate unification
-}
compTypeEq :: ComputationType -> ComputationType -> ElaborateM env ()
TComputation _ portTs1 pegT1 `compTypeEq` TComputation _ portTs2 pegT2 =
  sequence_
    [ unless (length portTs1 == length portTs2) mzero
    , zipWithM_ portEq portTs1 portTs2
    , pegT1 `pegEq` pegT2
    ]

portEq :: PortType -> PortType -> ElaborateM env ()
TPortNone _ valT1 `portEq` TPortNone _ valT2 = valT1 `typeEq` valT2
TPortSome _ adjT1 valT1 `portEq` TPortSome _ adjT2 valT2 =
  sequence_ [adjT1 `adjustmentEq` adjT2, valT1 `typeEq` valT2]
_ `portEq` _ = mzero

adjustmentEq :: AdjustmentType -> AdjustmentType -> ElaborateM env ()
TAdjustment _ interfaces1 `adjustmentEq` TAdjustment _ interfaces2 =
  unless (interfaces1 == interfaces2) mzero

pegEq :: PegType -> PegType -> ElaborateM env ()
TPeg _ abilT1 valT1 `pegEq` TPeg _ abilT2 valT2 =
  sequence_ [abilT1 `abilityEq` abilT2, valT1 `typeEq` valT2]

-- | Type environments distinguish monomorphic and polymorphic variables.
data ContextEntry
  = Mono ValueType
  | Poly PolyType
  deriving (Show)

emptyContext :: Context
emptyContext =
  Context
    { terms = Map.empty
    , ports = Map.empty
    , ambientAbility = emptyAbility generated
    , dataTypes = Map.empty
    , interfaces = Map.empty
    , rigidTypeVars = Map.empty
    , typeVarBindings = Map.empty
    }

-- | Elaboration monad.
type ElaborateM env = StateT Context (CompileM env)

[defpass|(from L1:L1RenamedAST to L2Elaborated)|]

-- | Elaborates the types in the module, producing a type-annotated AST.
elaborate :: (HasCallStack) => L1.Module -> CompileM env Module
elaborate = flip evalStateT emptyContext . descendModule xlate

xlate :: Xlate (ElaborateM env)
xlate =
  Xlate
    { onFnDefFnDef = \loc sig term -> do
        sig'@(FnSig _ _ _ (getPegValueType -> returnT)) <- descendFnSig xlate sig
        FnDef loc sig' <$> checkTerm term returnT
    , onTermEApplication = \loc comp args -> do
        (Typed _ term) <- inferApplication loc comp args
        return term
    , onTermEConLet = \loc bindings term ->
        EConLet loc
          <$> mapM (descendBinding xlate) bindings
          <*> inferTerm term
    , onTermEThunk = \loc comp -> do
        (Typed _ term) <- inferTerm (L1.EThunk loc comp)
        return term
    , onTermEInfixOp = \loc arg1 op arg2 -> do
        (Typed _ term) <- inferInfixOp loc arg1 op arg2
        return term
    , onComputationTermEComputation = \loc pats term ->
        EComputation loc
          <$> mapM (descendComputationPattern xlate) pats
          <*> inferTerm term
    , onBindingBindAnnotated = \loc name typ term -> do
        typ' <- descendValueType xlate typ
        BindAnnotated loc
          <$> descendName xlate name
          <*> checkTerm term typ'
    , onBindingBind = \loc name term ->
        BindAnnotated loc
          <$> descendName xlate name
          <*> inferTerm term
    , onPegTypeTPegNone = \loc valT ->
        TPeg loc (implicitAbilityVar loc) <$> descendValueType xlate valT
    , onPegTypeTPegSome = \loc abilT valT ->
        TPeg loc
          <$> descendAbilityType xlate abilT
          <*> descendValueType xlate valT
    , onGenericName = const Nothing
    , onInfixOpName = const Nothing
    , onAbilityType = const Nothing
    , onAbilityInitial = const Nothing
    , onAdjustmentType = const Nothing
    , onBinding = Just . registerBinding
    , onCommandSig = const Nothing
    , onComputationPattern = const Nothing
    , onComputationTerm = const Nothing
    , onComputationType = const Nothing
    , onConstructor = const Nothing
    , onConstructorName = const Nothing
    , onDataDef = const Nothing
    , onEffectVarName = const Nothing
    , onFnDef = const Nothing
    , onFnSig = const Nothing
    , onInterfaceDef = const Nothing
    , onInterfaceType = const Nothing
    , onModule = Just . elaborateModule
    , onName = const Nothing
    , onNamedParam = Just . registerParam
    , onNamedPort = Just . registerPort
    , onPegType = const Nothing
    , onPortType = const Nothing
    , onTerm = const Nothing
    , onTopDef = const Nothing
    , onTypeArgument = const Nothing
    , onTypeName = const Nothing
    , onTypeVarName = const Nothing
    , onTypeVariable = const Nothing
    , onValuePattern = const Nothing
    , onValueType = const Nothing
    }

elaborateModule :: L1.Module -> ElaborateM env Module
elaborateModule (L1.Module loc name names topDefs) = do
  -- Need to elaborate all topdef-names first, so they're available within all topdefs.
  mapM_ registerTopDefName topDefs
  -- Then, data types need to be elaborated, so their constructors are available within all topdefs.
  -- (But after topdef-names, since they may reference other topdefs).
  let (dataDefs, otherDefs) = partitionDataDefs topDefs
  dataDefs' <- mapM (descendDataDef xlate) dataDefs

  otherDefs' <- mapM (descendTopDef xlate) otherDefs
  let topDefs' = otherDefs' ++ map TopDataDef dataDefs'

  return $ Module loc name names topDefs'
 where
  registerTopDefName = \case
    (L1.TopFnDef (L1.FnDef _ sig@(L1.FnSig sigLoc name _ _) _)) -> do
      let sigToComputationType (L1.FnSig loc _ ports pegTyp) =
            TComputation loc <$> mapM getPortType ports <*> descendPegType xlate pegTyp
          getPortType (L1.NamedPort _ _ typ) = descendPortType xlate typ
      compT <- sigToComputationType sig
      let valT = TValueComputation sigLoc compT
      registerPolyVar sigLoc name valT
    (L1.TopDataDef dataDef) -> descendDataDef xlate dataDef >>= registerDataType
    (L1.TopInterfaceDef intDef) -> descendInterfaceDef xlate intDef >>= registerInterface

  partitionDataDefs = partitionWith filterDataDef
   where
    partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
    partitionWith _ [] = ([], [])
    partitionWith f (x : xs) = case f x of
      Left b -> (b : bs, cs)
      Right c -> (bs, c : cs)
     where
      (bs, cs) = partitionWith f xs

  filterDataDef (L1.TopDataDef def) = Left def
  filterDataDef def = Right def

registerParam :: L1.NamedParam -> ElaborateM env NamedParam
registerParam (L1.NamedParam loc name valTyp) = do
  valType <- descendValueType xlate valTyp
  registerMonoVar name valType
  NamedParam loc
    <$> descendName xlate name
    <*> pure valType

registerBinding :: L1.Binding -> ElaborateM env Binding
registerBinding binding = do
  binding'@(BindAnnotated _ name (Typed valT _)) <- inferBinding binding
  registerMonoVar name valT
  return binding'

registerPort :: L1.NamedPort -> ElaborateM env NamedPort
registerPort (L1.NamedPort loc name portTyp) = do
  portType <- descendPortType xlate portTyp
  registerPortVar name portType
  NamedPort loc
    <$> descendName xlate name
    <*> pure portType

inferTerm :: L1.Term -> ElaborateM env TypedTerm
inferTerm term = case term of
  L1.EIdent loc name -> do
    Typed
      <$> ( lookupVar name
              <|> lookupPortValueType name
              <|> (commandToValT <$> lookupCommand name)
              <|> emitPoisoned
          )
      <*> descendTerm xlate term
   where
    emitPoisoned = TPoisoned `withReportM` unknownIdentifier name
    commandToValT = TValueComputation loc . commandSigToComputationType
  L1.EIntLit loc _ -> Typed (TInt loc) <$> descendTerm xlate term
  L1.EUnit loc -> Typed (TUnit loc) <$> descendTerm xlate term
  L1.EApplication loc comp args -> inferApplication loc comp args
  L1.EInfixOp loc arg1 op arg2 -> inferInfixOp loc arg1 op arg2
  L1.EThunk{} -> todo "constructions cannot have their types inferred, change the grammar"
  L1.EConLet{} -> todo "constructions cannot have their types inferred, change the grammar"

inferInfixOp :: Loc -> L1.Term -> L1.InfixOpName -> L1.Term -> ElaborateM env TypedTerm
inferInfixOp loc arg1 (L1.InfixOpName name@(L1.GenName nameLoc _)) arg2 =
  inferTerm $ L1.EApplication loc (L1.EIdent nameLoc (L1.Name name)) [arg1, arg2]

inferApplication :: Loc -> L1.Term -> [L1.Term] -> ElaborateM env TypedTerm
inferApplication loc comp args = do
  typedComp@(Typed typ untypedComp) <- inferTerm comp
  ambAbility <- getAmbientAbility
  case typ of
    TValueComputation _ (TComputation _ portTs pegT@(getPegAbility -> pegAbility)) -> do
      (pegAbility `abilityEq` ambAbility)
        <|> reportM (abilityMismatch ambAbility untypedComp pegAbility)

      unless (length args == length portTs) $ reportM $ argumentMismatch args portTs

      typedArgs <- forM (zip args portTs) \(arg, portT) ->
        withAdjustment (getPortAdjustment portT) do
          checkTerm arg (getPortValueType portT)

      return $ Typed (getPegValueType pegT) (EApplication loc typedComp typedArgs)
    _ -> do
      reportM $ typeMismatch ExpectTComputation comp typ
      return $ Typed TPoisoned (EApplication loc typedComp [])

inferBinding :: L1.Binding -> ElaborateM env Binding
inferBinding binding = do
  (loc, name, typedTerm@(Typed typ _)) <- case binding of
    L1.Bind loc name term -> (loc,name,) <$> inferTerm term
    L1.BindAnnotated loc name typ term -> do
      typ' <- descendValueType xlate typ
      (loc,name,) <$> checkTerm term typ'
  name' <- descendName xlate name
  registerMonoVar name' typ
  return $ BindAnnotated loc name' typedTerm

checkTerm :: L1.Term -> ValueType -> ElaborateM env TypedTerm
checkTerm term expectedType = case term of
  L1.EThunk loc comps -> case expectedType of
    TValueComputation tLoc expectedCompT -> do
      comps' <- checkComputation comps expectedCompT
      return $ Typed (TValueComputation tLoc expectedCompT) (EThunk loc comps')
    _ ->
      Typed TPoisoned (EThunk loc [])
        `withReportM` typeMismatch ExpectTComputation term expectedType
  L1.EConLet loc bindings body -> do
    typedBindings <- mapM inferBinding bindings
    typedBody <- checkTerm body expectedType
    return $ Typed expectedType (EConLet loc typedBindings typedBody)
  _ -> do
    Typed actualType untypedTerm <- inferTerm term
    typ <-
      (actualType `typeEq` expectedType >> return expectedType)
        <|> ( TPoisoned
                `withReportM` typeMismatch (ExpectTVal expectedType) term actualType
            )

    return $ Typed typ untypedTerm

checkComputation ::
  [L1.ComputationTerm] ->
  ComputationType ->
  ElaborateM env [TypedComputationTerm]
checkComputation comps compT@(TComputation _ expectedPortTs peg) = do
  let ability = getPegAbility peg
      returnT = getPegValueType peg
  withAmbientAbility ability do
    -- TODO: check that every port is covered by the patterns in 'comps'
    map (TypedComp compT)
      <$> forM comps \(L1.EComputation loc pats body) ->
        EComputation loc
          <$> zipWithM bindComputationPattern pats expectedPortTs
          <*> checkTerm body returnT

bindComputationPattern :: L1.ComputationPattern -> PortType -> ElaborateM env ComputationPattern
bindComputationPattern pat portT@(getLoc -> portLoc) = case pat of
  L1.CompPatValue loc valPat ->
    CompPatValue loc
      <$> bindValuePattern valPat (getPortValueType portT)
  L1.CompPatRequest loc commandName valPats continuationName -> do
    let adjustment = getPortAdjustment portT
        ability = adjustAbility (emptyAbility portLoc) adjustment
    CommandSig _ _ params returnT <- lookupCommandInAbility ability commandName

    let paramTs = map getNamedParamType params
    typedValPats <- zipWithM bindValuePattern valPats paramTs

    continuationT <- createContinuationT returnT portT
    continuationName' <- descendName xlate continuationName
    registerMonoVar continuationName' continuationT

    commandName' <- descendName xlate commandName
    return $ CompPatRequest loc commandName' typedValPats continuationName'
  L1.CompPatCatchAll loc name -> do
    adjustedAbility <- adjustAmbientAbility (getPortAdjustment portT)
    let compT = TComputation portLoc [] (TPeg portLoc adjustedAbility (getPortValueType portT))
    name' <- descendName xlate name
    registerMonoVar name' (TValueComputation portLoc compT)
    return $ CompPatCatchAll loc name'

-- | Creates the type of a continuation within a port.
createContinuationT :: ValueType -> PortType -> ElaborateM env ValueType
createContinuationT commandReturnT port = do
  ability <- adjustAmbientAbility (getPortAdjustment port)
  return $
    TValueComputation generated $
      TComputation
        generated
        [TPortNone generated commandReturnT]
        (TPeg generated ability (getPortValueType port))

bindValuePattern :: L1.ValuePattern -> ValueType -> ElaborateM env ValuePattern
bindValuePattern (L1.ValPat loc varName []) expectedType = do
  varName' <- descendName xlate varName
  registerMonoVar varName' expectedType
  return $ ValPat loc varName' []
bindValuePattern pat@(L1.ValPat loc constructorName pats) expectedType = do
  (Constructor _ _ paramTs, dataType) <- lookupConstructor constructorName
  (dataType `typeEq` expectedType)
    <|> reportM (valPatternMismatch (ExpectTVal expectedType) pat dataType)
  ValPat loc
    <$> descendName xlate constructorName
    <*> zipWithM bindValuePattern pats paramTs

-- | The ability which contains no commands.
emptyAbility :: Loc -> AbilityType
emptyAbility loc = TAbilityInterfaces loc AbilityInitEmpty []

-- | Extracts the type of a named parameter.
getNamedParamType :: NamedParam -> ValueType
getNamedParamType (NamedParam _ _ t) = t

{- | Extracts the ability of the given peg
(using the implicit effect variable if none are specified).
-}
getPegAbility :: PegType -> AbilityType
getPegAbility (TPeg _ abilT _) = abilT

{- | Extracts the adjustment of the given port
(using the implicit effect variable if none are specified).
-}
getPortAdjustment :: PortType -> AdjustmentType
getPortAdjustment (TPortNone loc _) = TAdjustment loc []
getPortAdjustment (TPortSome _ adjT _) = adjT

-- | Extracts the value type of the given peg.
getPegValueType :: PegType -> ValueType
getPegValueType (TPeg _ _ valT) = valT

-- | Extracts the value type of the given port.
getPortValueType :: PortType -> ValueType
getPortValueType (TPortNone _ valT) = valT
getPortValueType (TPortSome _ _ valT) = valT

-- | The effect type variable that is implicit in all ports/pegs when none is specified.
implicitEffectVar :: EffectVarName
implicitEffectVar = EffectVarName $ GenName generated "ε"

{- | The ability that is implicit when none is specified, parameterized such that
it may take on any effects.
-}
implicitAbilityVar :: Loc -> AbilityType
implicitAbilityVar loc =
  TAbilityInterfaces loc (AbilityInitEffectVar generated implicitEffectVar) []

-- | The closed ability permits no effects.
closedAbility :: Loc -> AbilityType
closedAbility loc =
  TAbilityInterfaces loc AbilityInitEmpty []