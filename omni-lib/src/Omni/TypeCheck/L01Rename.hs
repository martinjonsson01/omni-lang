{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans
  -Wno-name-shadowing
  -Wno-unused-top-binds
  -Wno-redundant-constraints #-}

module Omni.TypeCheck.L01Rename (renameIdents) where

import Control.Monad.State
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Nanopass (defpass)
import Omni.Imports
import Omni.Locations
import Omni.Name qualified as Name
import Omni.Reporting qualified as Reporting
import Omni.TypeCheck.L00AST qualified as L0
import Omni.TypeCheck.L01RenamedAST

-- | A namestore which keeps track of which names are used in which scopes.
data Names = Names
  { getScopes :: NonEmpty Scope
  , usedNames :: Set Name
  }

emptyNames :: Names
emptyNames = Names (NE.singleton Map.empty) Set.empty

-- | Mapping parser names in the current scope to their (module-)globally unique names.
type Scope = Map Text Name

type RenameM env = StateT Names (CompileM env)

-- | Tries to find the name in any current scope.
lookupName :: (L0.ToGenericName n) => n -> RenameM env (Maybe Name)
lookupName (L0.toGeneric -> (L0.GenName _ name)) = do
  scopes <- gets getScopes
  lookForNameInScopes scopes
 where
  lookForNameInScopes :: NonEmpty Scope -> RenameM env (Maybe Name)
  lookForNameInScopes (scope NE.:| mbScopes) =
    case Map.lookup name scope of
      Just renamed -> return $ Just renamed
      Nothing -> case NE.nonEmpty mbScopes of
        Just scopes -> lookForNameInScopes scopes
        Nothing -> return Nothing

-- | Tries to find the name only in the local scope.
lookupNameInLocalScope :: (L0.ToGenericName n) => n -> RenameM env (Maybe Name)
lookupNameInLocalScope (L0.toGeneric -> (L0.GenName _ name)) = do
  scope NE.:| _ <- gets getScopes
  case Map.lookup name scope of
    Just renamed -> return $ Just renamed
    Nothing -> return Nothing

-- | Checks whether the given name exists in this module.
findGlobalName :: Name.Ident -> RenameM env (Maybe Name)
findGlobalName ident = do
  taken <- gets usedNames
  return $ find (\(Name (GenName _ takenIdent)) -> takenIdent == ident) taken

-- | Converts the given identifier to a (module-)globally unique name.
toUniqueName :: Name.Ident -> RenameM env Name.Ident
toUniqueName (Name.Ident name) = findUnique Nothing
 where
  findUnique :: Maybe Int -> RenameM env Name.Ident
  findUnique counter = do
    let potentialName = Name.Ident $ name <> maybe "" (Text.pack . show) counter
    mbGlobal <- findGlobalName potentialName
    if isNothing mbGlobal
      then return potentialName
      else
        let updatedCounter = case counter of
              Nothing -> Just 0
              Just n -> Just (n + 1)
         in findUnique updatedCounter

-- | Registers the given name to the current scope (without checking).
registerRename :: (L0.ToGenericName n) => n -> Name -> RenameM env ()
registerRename (L0.toGeneric -> (L0.GenName _ oldName)) renamed = do
  (scope NE.:| scopes) <- gets getScopes
  let updatedScope = Map.insert oldName renamed scope
  modify $ \s ->
    s
      { getScopes = updatedScope NE.:| scopes
      , usedNames = Set.insert renamed (usedNames s)
      }

{- | Registers the given identifier as a (module-)globally unique
name, renaming it if it's only unique in the current scope but not globally.
-}
registerUniqueName :: (L0.ToGenericName n) => n -> RenameM env Name
registerUniqueName ident@(L0.toGeneric -> (L0.GenName loc name)) = do
  mbDuplicate <- lookupNameInLocalScope ident
  case mbDuplicate of
    Just (Name (GenName duplicateLoc _)) ->
      (lift . report . Reporting.duplicateIdentifier name)
        (fromList [loc, duplicateLoc])
    Nothing -> do
      mbShadowed <- lookupName ident
      case mbShadowed of
        Just shadowed -> lift $ report $ Reporting.shadowedIdentifier ident shadowed
        Nothing -> pure ()

  let name' = Name.Ident name
  mbGlobal <- findGlobalName name'
  newName <-
    case mbGlobal of
      Just _ -> toUniqueName name'
      Nothing -> return $ Name.Ident name
  let renamed = Name $ GenName loc newName
  registerRename ident renamed
  return renamed

{- | Renames the identifier to the new name it has been given in the current
scope, or if none exists reports an error.
-}
rename :: (L0.ToGenericName n) => n -> RenameM env Name.Ident
rename ident@(L0.toGeneric -> (L0.GenName _ name)) = do
  mbRename <- lookupName ident
  case mbRename of
    Just (Name (GenName _ existingRename)) -> return existingRename
    Nothing ->
      lift $ Name.Ident name `withReport` Reporting.unknownIdentifier ident

-- | Performs the given computation inside a nested scope.
nested :: RenameM env a -> RenameM env a
nested m = do
  scopes <- gets getScopes
  modify $ \s -> s{getScopes = Map.empty NE.<| scopes}
  result <- m
  modify $ \s -> s{getScopes = scopes}
  return result

[defpass|(from L0:L0AST to L1RenamedAST)|]

-- | Renames overloaded identifiers such that they get unique names.
renameIdents :: L0.Module -> CompileM env Module
renameIdents =
  flip evalStateT emptyNames . descendModule xlate
 where
  xlate =
    Xlate
      { onGenericNameGenName =
          \loc name -> GenName loc <$> rename (L0.GenName loc name)
      , onGenericName = const Nothing
      , onAbilityType = const Nothing
      , onAdjustmentType = const Nothing
      , onBinding = const Nothing
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
      , onModule = Just . registerModule
      , onName = const Nothing
      , onNamedParam = Just . registerParam
      , onNamedPort = Just . registerPort
      , onPegType = const Nothing
      , onPortType = const Nothing
      , onTerm = const Nothing
      , onTopDef = Just . registerTopDef
      , onTypeArgument = const Nothing
      , onTypeName = const Nothing
      , onTypeVarName = const Nothing
      , onTypeVariable = const Nothing
      , onValuePattern = const Nothing
      , onValueType = const Nothing
      }

  registerModule (L0.Module loc name topDefs) = do
    let topDefNames = map getTopDefName topDefs
    mapM_ registerUniqueName topDefNames
    topDefs' <- mapM (descendTopDef xlate) topDefs
    return (Module loc name topDefs')
   where
    getTopDefName = \case
      (L0.TopFnDef (L0.FnDef _ (L0.FnSig _ name _ _) _)) -> L0.toGeneric name
      (L0.TopDataDef (L0.DataDef _ name _ _)) -> L0.toGeneric name
      (L0.TopInterfaceDef (L0.InterfaceDef _ name _ _)) -> L0.toGeneric name

  registerTopDef = \case
    (L0.TopFnDef fnDef) ->
      nested $ TopFnDef <$> descendFnDef xlate fnDef
    (L0.TopDataDef dataDef) ->
      nested $ TopDataDef <$> descendDataDef xlate dataDef
    (L0.TopInterfaceDef intDef) ->
      nested $ TopInterfaceDef <$> descendInterfaceDef xlate intDef

  registerParam (L0.NamedParam loc name valTyp) = do
    name' <- registerUniqueName name
    NamedParam loc name' <$> descendValueType xlate valTyp

  registerPort (L0.NamedPort loc name valTyp) = do
    name' <- registerUniqueName name
    NamedPort loc name' <$> descendPortType xlate valTyp
