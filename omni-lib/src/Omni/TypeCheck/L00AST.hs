{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Omni.TypeCheck.L00AST where

import Language.Nanopass (deflang)
import Omni.Abs qualified as Parsed
import Omni.Locations
import Omni.Name qualified as Name

[deflang|
((L0AST ident)
  (Module
    (Module Loc Name:Module (* TopDef))
  )
  (TopDef
    (FnDef Loc Name ParamList Type Exp)
  )
  (ParamList 
    (ParamList Loc (* Param))
  )
  (Param
    (Param Loc Name Type)
  )
  (Type
    (TFn Loc (* Type) Type)
    (TUnit Loc)
    (TInt Loc)
    (TNamed Loc Name)
  )
  (Exp
    (EIdent Loc Name)
    (EIntLit Loc Integer)
    (EUnit Loc)
    (EInfixOp Loc Exp Parsed:InfixOpIdent Exp)
    (EApplication Loc Exp (* Exp))
  )
  (Name 
    (Name Loc ident)
  )
)
|]
deriving instance (Show ident) => Show (Module ident)
deriving instance (Show ident) => Show (TopDef ident)
deriving instance (Show ident) => Show (ParamList ident)
deriving instance (Show ident) => Show (Param ident)
deriving instance (Show ident) => Show (Type ident)
deriving instance (Show ident) => Show (Name ident)

deriving instance (Show ident) => Show (Exp ident)

deriving instance (Eq ident) => Eq (Name ident)
deriving instance (Ord ident) => Ord (Name ident)

deriving instance Functor Module
deriving instance Functor TopDef
deriving instance Functor ParamList
deriving instance Functor Param
deriving instance Functor Type
deriving instance Functor Exp
deriving instance Functor Name

deriving instance Foldable Module
deriving instance Foldable TopDef
deriving instance Foldable ParamList
deriving instance Foldable Param
deriving instance Foldable Type
deriving instance Foldable Exp
deriving instance Foldable Name

deriving instance Traversable Module
deriving instance Traversable TopDef
deriving instance Traversable ParamList
deriving instance Traversable Param
deriving instance Traversable Type
deriving instance Traversable Exp
deriving instance Traversable Name

instance Located (Module ident) where
  getLoc (Module loc _ _) = loc

instance Located (TopDef ident) where
  getLoc = \case
    FnDef loc _ _ _ _ -> loc

instance Located (ParamList ident) where
  getLoc (ParamList loc _) = loc

instance Located (Param ident) where
  getLoc (Param loc _ _) = loc

instance Located (Type ident) where
  getLoc = \case
    TUnit loc -> loc
    TInt loc -> loc
    TFn loc _ _ -> loc
    TNamed loc _ -> loc

instance Located (Exp ident) where
  getLoc = \case
    EIdent loc _ -> loc
    EApplication loc _ _ -> loc
    EInfixOp loc _ _ _ -> loc
    EIntLit loc _ -> loc
    EUnit loc -> loc

instance Located (Name ident) where
  getLoc (Name loc _) = loc
