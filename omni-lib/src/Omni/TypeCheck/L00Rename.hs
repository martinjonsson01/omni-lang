{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Omni.TypeCheck.L00Rename (renameIdents) where

import Control.Monad.State
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Omni.Abs qualified as Parsed
import Omni.Imports
import Omni.Locations qualified as Locs
import Omni.Name qualified as Name
import Omni.Reporting qualified as Reporting
import Omni.TypeCheck.L00AST qualified as L0

-- | A namestore which keeps track of which names are used in which scopes.
data Names = Names
  { getScopes :: NonEmpty Scope
  , usedNames :: Set (L0.Name Name.Ident)
  }

emptyNames :: Names
emptyNames = Names (NE.singleton Map.empty) Set.empty

-- | Mapping parser names in the current scope to their (module-)globally unique names.
type Scope = Map Parsed.Ident (L0.Name Name.Ident)

type RenameM env = StateT Names (CompileM env)

-- | Tries to find the name in any current scope.
lookupName :: L0.Name Parsed.Ident -> RenameM env (Maybe (L0.Name Name.Ident))
lookupName (L0.Name _ name) = do
  scopes <- gets getScopes
  lookForNameInScopes scopes
 where
  lookForNameInScopes :: NonEmpty Scope -> RenameM env (Maybe (L0.Name Name.Ident))
  lookForNameInScopes (scope NE.:| mbScopes) =
    case Map.lookup name scope of
      Just renamed -> return $ Just renamed
      Nothing -> case NE.nonEmpty mbScopes of
        Just scopes -> lookForNameInScopes scopes
        Nothing -> return Nothing

-- | Tries to find the name only in the local scope.
lookupNameInLocalScope :: L0.Name Parsed.Ident -> RenameM env (Maybe (L0.Name Name.Ident))
lookupNameInLocalScope (L0.Name _ name) = do
  scope NE.:| _ <- gets getScopes
  case Map.lookup name scope of
    Just renamed -> return $ Just renamed
    Nothing -> return Nothing

-- | Checks whether the given name exists in this module.
findGlobalName :: Name.Ident -> RenameM env (Maybe (L0.Name Name.Ident))
findGlobalName ident = do
  taken <- gets usedNames
  return $ find (\(L0.Name _ takenIdent) -> takenIdent == ident) taken

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
registerRename :: L0.Name Parsed.Ident -> L0.Name Name.Ident -> RenameM env ()
registerRename (L0.Name _ oldName) renamed = do
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
registerUniqueName :: L0.Name Parsed.Ident -> RenameM env (L0.Name Name.Ident)
registerUniqueName ident@(L0.Name loc (Parsed.Ident name)) = do
  mbDuplicate <- lookupNameInLocalScope ident
  case mbDuplicate of
    Just (L0.Name duplicateLoc _) ->
      (lift . report . Reporting.duplicateIdentifier name)
        (Locs.fromList [loc, duplicateLoc])
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
  let renamed = L0.Name loc newName
  registerRename ident renamed
  return renamed

{- | Renames the identifier to the new name it has been given in the current
scope, or if none exists reports an error.
-}
rename :: L0.Name Parsed.Ident -> RenameM env (L0.Name Name.Ident)
rename ident@(L0.Name loc (Parsed.Ident name)) = do
  mbRename <- lookupName ident
  case mbRename of
    Just existingRename -> return existingRename
    Nothing ->
      lift $ L0.Name loc (Name.Ident name) `withReport` Reporting.unknownIdentifier ident

-- | Performs the given computation inside a nested scope.
nested :: RenameM env a -> RenameM env a
nested m = do
  scopes <- gets getScopes
  modify $ \s -> s{getScopes = Map.empty NE.<| scopes}
  result <- m
  modify $ \s -> s{getScopes = scopes}
  return result

-- | Renames overloaded identifiers such that they get unique names.
renameIdents :: L0.Module Parsed.Ident -> CompileM env (L0.Module Name.Ident)
renameIdents (L0.Module loc name topDefs) =
  flip evalStateT emptyNames $ do
    let topDefNames = map (\(L0.FnDef _ name _ _ _) -> name) topDefs
    mapM_ registerUniqueName topDefNames
    renamedTopDefs <- mapM renameIdentsTopDef topDefs
    return $ L0.Module loc name renamedTopDefs
 where
  renameIdentsTopDef :: L0.TopDef Parsed.Ident -> RenameM env (L0.TopDef Name.Ident)
  renameIdentsTopDef = \case
    L0.FnDef loc name paramList returnTyp body -> do
      name' <- rename name
      (paramList', returnTyp', body') <- nested do
        paramList' <- registerParams paramList
        returnTyp' <- renameType returnTyp
        body' <- renameExp body
        return (paramList', returnTyp', body')
      return $ L0.FnDef loc name' paramList' returnTyp' body'

  registerParams :: L0.ParamList Parsed.Ident -> RenameM env (L0.ParamList Name.Ident)
  registerParams (L0.ParamList loc params) =
    L0.ParamList loc <$> mapM registerParam params
   where
    registerParam :: L0.Param Parsed.Ident -> RenameM env (L0.Param Name.Ident)
    registerParam (L0.Param loc ident typ) = do
      ident' <- registerUniqueName ident
      typ' <- renameType typ
      return (L0.Param loc ident' typ')

  renameType :: L0.Type Parsed.Ident -> RenameM env (L0.Type Name.Ident)
  renameType = \case
    L0.TUnit loc -> return $ L0.TUnit loc
    L0.TInt loc -> return $ L0.TUnit loc
    L0.TFn loc argTs returnTyp ->
      L0.TFn loc
        <$> mapM renameType argTs
        <*> renameType returnTyp
    L0.TNamed loc ident -> L0.TNamed loc <$> rename ident

  renameExp :: L0.Exp Parsed.Ident -> RenameM env (L0.Exp Name.Ident)
  renameExp = \case
    L0.EIdent loc ident -> L0.EIdent loc <$> rename ident
    L0.EApplication loc eFun eArgs ->
      L0.EApplication loc
        <$> renameExp eFun
        <*> mapM renameExp eArgs
    L0.EInfixOp loc e1 op e2 ->
      L0.EInfixOp loc
        <$> renameExp e1
        <*> pure op
        <*> renameExp e2
    L0.EIntLit loc i -> return $ L0.EIntLit loc i
    L0.EUnit loc -> return $ L0.EUnit loc
