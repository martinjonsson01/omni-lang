module Omni.TypeCheck.L00ParseAST (convertParsed) where

import Omni.Abs qualified as Parsed
import Omni.Locations
import Omni.Monad
import Omni.Name qualified as Name
import Omni.TypeCheck.L00AST

{- | Converts the given AST into its corresponding internal representation.

This is only necessary because the BNFC-generated AST and the
nanopass-generated AST are not automatically compatible.
-}
convertParsed :: FilePath -> Parsed.Module -> CompileM env (Module Parsed.Ident)
convertParsed file = \case
  Parsed.Module loc' (Parsed.Name _ (Parsed.Ident name)) topDefs ->
    let loc = toLoc loc'
        moduleName = Name.Module name
     in return $
          Module
            loc
            moduleName
            (map convertTopDef topDefs)
 where
  toLoc = convertPos file

  convertName :: Parsed.Name -> Name Parsed.Ident
  convertName (Parsed.Name loc ident) = Name (toLoc loc) ident

  convertTopDef :: Parsed.TopDef -> TopDef Parsed.Ident
  convertTopDef = \case
    Parsed.FnDef loc name params returnType body ->
      FnDef
        (toLoc loc)
        (convertName name)
        (convertParamList params)
        (convertType returnType)
        (convertExp body)

  convertParamList :: Parsed.ParamList -> ParamList Parsed.Ident
  convertParamList = \case
    Parsed.ParamList loc params -> ParamList (toLoc loc) (map convertParam params)

  convertParam :: Parsed.Param -> Param Parsed.Ident
  convertParam (Parsed.Param loc name typ) =
    Param
      (toLoc loc)
      (convertName name)
      (convertType typ)

  convertType :: Parsed.Type ->  Type Parsed.Ident
  convertType = \case
    Parsed.TFn loc paramTyps returnTyp ->
      TFn
        (toLoc loc)
        (map convertType paramTyps)
        (convertType returnTyp)
    Parsed.TUnit loc -> TUnit $ toLoc loc
    Parsed.TInt loc -> TInt $ toLoc loc
    Parsed.TNamed loc name -> TNamed (toLoc loc) (convertName name)

  convertExp :: Parsed.Exp -> Exp Parsed.Ident
  convertExp = \case
    Parsed.EIdent loc ident -> EIdent (toLoc loc) (convertName ident)
    Parsed.EIntLit loc i -> EIntLit (toLoc loc) i
    Parsed.EUnit loc -> EUnit (toLoc loc)
    Parsed.EInfixOp loc e1 op e2 -> EInfixOp (toLoc loc) (convertExp e1) op (convertExp e2)
    Parsed.EApplication loc fnExp argExps ->
      EApplication
        (toLoc loc)
        (convertExp fnExp)
        (map convertExp argExps)
