{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans
  -Wno-name-shadowing
  -Wno-unused-top-binds
  -Wno-redundant-constraints #-}

module Omni.TypeCheck.L01Rename (renameIdents, Names (..)) where

import Control.Monad.State
import Data.Char (isLower)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Language.Nanopass (defpass)
import Omni.Imports
import Omni.Locations
import Omni.Name (ToGenericName (..))
import Omni.Name qualified as Name
import Omni.Reporting qualified as Reporting
import Omni.TypeCheck.L00AST qualified as L0
import Omni.TypeCheck.L01RenamedAST

-- | Renaming monad.
type RenameM env = StateT Names (CompileM env)

-- | Tries to find the name in any current scope.
lookupName :: (ToGenericName n) => n -> RenameM env (Maybe Name)
lookupName (toGeneric -> (Name.GenName _ name)) = do
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
lookupNameInLocalScope :: (ToGenericName n) => n -> RenameM env (Maybe Name)
lookupNameInLocalScope (toGeneric -> (Name.GenName _ name)) = do
  scope NE.:| _ <- gets getScopes
  case Map.lookup name scope of
    Just renamed -> return $ Just renamed
    Nothing -> return Nothing

-- | Checks whether the given name exists in this module.
findGlobalName :: Name.Ident -> RenameM env (Maybe Name)
findGlobalName ident = do
  taken <- gets (Map.keysSet . usedNames)
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

{- | Registers the given identifier as a (module-)globally unique
name, renaming it if it's only unique in the current scope but not globally.
-}
registerUniqueName :: (ToGenericName n) => n -> RenameM env Name
registerUniqueName ident@(toGeneric -> (Name.GenName loc name)) = do
  mbDuplicate <- lookupNameInLocalScope ident
  case mbDuplicate of
    Just (Name (GenName duplicateLoc _)) ->
      (reportM . Reporting.duplicateIdentifier name)
        (fromList [loc, duplicateLoc])
    Nothing -> do
      mbShadowed <- lookupName ident
      case mbShadowed of
        Just shadowed -> reportM $ Reporting.shadowedIdentifier ident shadowed
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
 where
  -- \| Registers the given name to the current scope (without checking).
  registerRename :: (ToGenericName n) => n -> Name -> RenameM env ()
  registerRename (toGeneric -> (Name.GenName _ oldName)) renamed = do
    (scope NE.:| scopes) <- gets getScopes
    let updatedScope = Map.insert oldName renamed scope
    modify $ \s ->
      s
        { getScopes = updatedScope NE.:| scopes
        , usedNames = Map.insert renamed oldName (usedNames s)
        }

{- | Renames the identifier to the new name it has been given in the current
scope, or if none exists reports an error.
-}
rename :: (ToGenericName n) => n -> RenameM env Name.Ident
rename ident@(toGeneric -> (Name.GenName _ name)) = do
  mbRename <- lookupName ident
  case mbRename of
    Just (Name (GenName _ existingRename)) -> return existingRename
    Nothing -> 
      lift $ Name.Ident name `withReport` Reporting.unknownIdentifier ident

-- | Performs the given computation inside a nested namespace.
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
      , onInfixOpName = const Nothing
      , onAbilityType = const Nothing
      , onAbilityInitial = const Nothing
      , onAdjustmentType = const Nothing
      , onBinding = Just . registerBinding
      , onCommandSig = const Nothing
      , onComputationPattern = const Nothing
      , onComputationTerm = const Nothing
      , onComputationType = const Nothing
      , onConstructor = Just . registerConstructor
      , onConstructorName = const Nothing
      , onDataDef = const Nothing
      , onEffectVarName = const Nothing
      , onFnDef = const Nothing
      , onFnSig = const Nothing
      , onInterfaceDef = const Nothing
      , onInterfaceType = const Nothing
      , onModuleModule = convertModule
      , onModule = const Nothing
      , onName = const Nothing
      , onNamedParam = Just . registerParam
      , onNamedPort = Just . registerPort
      , onPegType = const Nothing
      , onPortType = const Nothing
      , onTerm = const Nothing
      , onTopDef = const Nothing
      , onTypeArgument = const Nothing
      , onTypeName = const Nothing
      , onTypeVarName = Just . convertTypeVarName
      , onTypeVariable = const Nothing
      , onValuePattern = Just . registerValuePattern
      , onValueType = const Nothing
      }

  convertModule loc name topDefs = do
    -- Need to register all topdef-names first, so they're available within all topdefs.
    mapM_ registerTopDefName topDefs
    -- Then, data types need to be registered, so their constructors are available within all topdefs.
    -- (But after topdef-names, since they may reference other topdefs).
    let (dataDefs, otherDefs) = partitionDataDefs topDefs
    dataDefs' <- mapM (descendDataDef xlate) dataDefs

    otherDefs' <- mapM (nested . descendTopDef xlate) otherDefs
    let topDefs' = otherDefs' ++ map TopDataDef dataDefs'

    names <- get
    return $ Module loc name names topDefs'
   where
    registerTopDefName = \case
      (L0.TopFnDef (L0.FnDef _ (L0.FnSig _ name _ _) _)) -> registerUniqueName name
      (L0.TopDataDef (L0.DataDef _ typName _ _)) -> registerUniqueName typName
      (L0.TopInterfaceDef (L0.InterfaceDef _ typName _ _)) -> registerUniqueName typName

    partitionDataDefs = partitionWith filterDataDef
     where
      partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
      partitionWith _ [] = ([], [])
      partitionWith f (x : xs) = case f x of
        Left b -> (b : bs, cs)
        Right c -> (bs, c : cs)
       where
        (bs, cs) = partitionWith f xs

    filterDataDef (L0.TopDataDef def) = Left def
    filterDataDef def = Right def

  registerParam (L0.NamedParam loc name valTyp) = do
    NamedParam loc
      <$> registerUniqueName name
      <*> descendValueType xlate valTyp

  registerPort (L0.NamedPort loc name valTyp) = do
    NamedPort loc
      <$> registerUniqueName name
      <*> descendPortType xlate valTyp

  registerBinding = \case
    L0.Bind loc name val -> Bind loc <$> registerUniqueName name <*> descendTerm xlate val
    L0.BindAnnotated loc name valTyp val ->
      BindAnnotated loc
        <$> registerUniqueName name
        <*> descendValueType xlate valTyp
        <*> descendTerm xlate val

  registerValuePattern (L0.ValPat loc name@(L0.Name (L0.GenName _ nameText)) pats)
    | Just (firstLetter, _) <- Text.uncons nameText
    , isLower firstLetter = do
        ValPat loc
          <$> registerUniqueName name
          <*> mapM registerValuePattern pats
    | otherwise =
        ValPat loc
          <$> descendName xlate name
          <*> mapM registerValuePattern pats

  registerConstructor (L0.Constructor loc (L0.ConstructorName name) params) =
    Constructor
      loc
      . toConstructorName
      <$> registerUniqueName name
      <*> mapM (descendValueType xlate) params
   where
    toConstructorName (Name genName) = ConstructorName genName

  -- \| Type variables need not be renamed.
  convertTypeVarName (L0.TypeVarName (L0.GenName loc name)) =
    return $ TypeVarName $ GenName loc (Name.Ident name)
