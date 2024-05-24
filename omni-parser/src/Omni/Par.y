-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.6).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Omni.Par
  ( happyError
  , myLexer
  , pModule
  ) where

import Prelude

import qualified Omni.Abs
import Omni.Lex
import qualified Data.Text

}

%name pModule_internal Module
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('            { PT _ _ (TS _ 1)           }
  ')'            { PT _ _ (TS _ 2)           }
  ','            { PT _ _ (TS _ 3)           }
  '->'           { PT _ _ (TS _ 4)           }
  ':'            { PT _ _ (TS _ 5)           }
  '<'            { PT _ _ (TS _ 6)           }
  '='            { PT _ _ (TS _ 7)           }
  '>'            { PT _ _ (TS _ 8)           }
  'Int'          { PT _ _ (TS _ 9)           }
  'Unit'         { PT _ _ (TS _ 10)          }
  '['            { PT _ _ (TS _ 11)          }
  ']'            { PT _ _ (TS _ 12)          }
  'data'         { PT _ _ (TS _ 13)          }
  'in'           { PT _ _ (TS _ 14)          }
  'infixl'       { PT _ _ (TS _ 15)          }
  'interface'    { PT _ _ (TS _ 16)          }
  'let'          { PT _ _ (TS _ 17)          }
  'module'       { PT _ _ (TS _ 18)          }
  '{'            { PT _ _ (TS _ 19)          }
  '|'            { PT _ _ (TS _ 20)          }
  '}'            { PT _ _ (TS _ 21)          }
  L_integ        { PT _ _ (TI _)             }
  L_UpperIdent   { PT _ _ (T_UpperIdent _)   }
  L_LowerIdent   { PT _ _ (T_LowerIdent _)   }
  L_InfixOpIdent { PT _ _ (T_InfixOpIdent _) }

%%

Integer :: { (Omni.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Omni.Abs.BNFC'Position (tokenSpan $1), (read (Data.Text.unpack (tokenText $1))) :: Integer) }

UpperIdent :: { (Omni.Abs.BNFC'Position, Omni.Abs.UpperIdent) }
UpperIdent  : L_UpperIdent { (uncurry Omni.Abs.BNFC'Position (tokenSpan $1), Omni.Abs.UpperIdent (tokenText $1)) }

LowerIdent :: { (Omni.Abs.BNFC'Position, Omni.Abs.LowerIdent) }
LowerIdent  : L_LowerIdent { (uncurry Omni.Abs.BNFC'Position (tokenSpan $1), Omni.Abs.LowerIdent (tokenText $1)) }

InfixOpIdent :: { (Omni.Abs.BNFC'Position, Omni.Abs.InfixOpIdent) }
InfixOpIdent  : L_InfixOpIdent { (uncurry Omni.Abs.BNFC'Position (tokenSpan $1), Omni.Abs.InfixOpIdent (tokenText $1)) }

TypeName :: { (Omni.Abs.BNFC'Position, Omni.Abs.TypeName) }
TypeName
  : UpperIdent { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TypeName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

ConstructorName :: { (Omni.Abs.BNFC'Position, Omni.Abs.ConstructorName) }
ConstructorName
  : UpperIdent { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.ConstructorName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Name :: { (Omni.Abs.BNFC'Position, Omni.Abs.Name) }
Name
  : UpperIdent { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.UpperName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | LowerIdent { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.LowerName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

InfixOpName :: { (Omni.Abs.BNFC'Position, Omni.Abs.InfixOpName) }
InfixOpName
  : InfixOpIdent { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.InfixOpName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

TypeVarName :: { (Omni.Abs.BNFC'Position, Omni.Abs.TypeVarName) }
TypeVarName
  : LowerIdent { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TypeVarName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

EffectVarName :: { (Omni.Abs.BNFC'Position, Omni.Abs.EffectVarName) }
EffectVarName
  : LowerIdent { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.EffectVarName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

FnName :: { (Omni.Abs.BNFC'Position, Omni.Abs.FnName) }
FnName
  : Name { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.OrdinaryFnName (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | 'infixl' InfixOpIdent { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $2), Omni.Abs.InfixFnName (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $2)) (snd $2)) }

Module :: { (Omni.Abs.BNFC'Position, Omni.Abs.Module) }
Module
  : 'module' UpperIdent '(' ')' ListTopDef { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $5), Omni.Abs.Module (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $5)) (snd $2) (snd $5)) }

TopDef :: { (Omni.Abs.BNFC'Position, Omni.Abs.TopDef) }
TopDef
  : FnDef { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TopFnDef (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | DataDef { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TopDataDef (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | InterfaceDef { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TopInterfaceDef (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

ListTopDef :: { (Omni.Abs.BNFC'Position, [Omni.Abs.TopDef]) }
ListTopDef
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | TopDef ListTopDef { (Omni.Abs.spanBNFC'Position (fst $1) (fst $2), (:) (snd $1) (snd $2)) }

FnDef :: { (Omni.Abs.BNFC'Position, Omni.Abs.FnDef) }
FnDef
  : FnSig '=' Term { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), Omni.Abs.FnDef (Omni.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $3)) }

FnSig :: { (Omni.Abs.BNFC'Position, Omni.Abs.FnSig) }
FnSig
  : FnName '(' ListNamedPort ')' ':' PegType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $6), Omni.Abs.FnSig (Omni.Abs.spanBNFC'Position (fst $1) (fst $6)) (snd $1) (snd $3) (snd $6)) }

NamedPort :: { (Omni.Abs.BNFC'Position, Omni.Abs.NamedPort) }
NamedPort
  : Name ':' PortType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), Omni.Abs.NamedPort (Omni.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $3)) }

ListNamedPort :: { (Omni.Abs.BNFC'Position, [Omni.Abs.NamedPort]) }
ListNamedPort
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | NamedPort { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | NamedPort ',' ListNamedPort { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

DataDef :: { (Omni.Abs.BNFC'Position, Omni.Abs.DataDef) }
DataDef
  : 'data' TypeName TypeVarList ConstructorList { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4), Omni.Abs.DataDef (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4)) (snd $2) (snd $3) (snd $4)) }

ConstructorList :: { (Omni.Abs.BNFC'Position, Omni.Abs.ConstructorList) }
ConstructorList
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, Omni.Abs.NoConstructors Omni.Abs.BNFC'NoPosition) }
  | '=' ListConstructor { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $2), Omni.Abs.SomeConstructors (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $2)) (snd $2)) }

Constructor :: { (Omni.Abs.BNFC'Position, Omni.Abs.Constructor) }
Constructor
  : ConstructorName ValueTypeList { (Omni.Abs.spanBNFC'Position (fst $1) (fst $2), Omni.Abs.Constructor (Omni.Abs.spanBNFC'Position (fst $1) (fst $2)) (snd $1) (snd $2)) }

ListConstructor :: { (Omni.Abs.BNFC'Position, [Omni.Abs.Constructor]) }
ListConstructor
  : Constructor { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | Constructor '|' ListConstructor { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

InterfaceDef :: { (Omni.Abs.BNFC'Position, Omni.Abs.InterfaceDef) }
InterfaceDef
  : 'interface' TypeName TypeVarList CommandSigList { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4), Omni.Abs.InterfaceDef (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4)) (snd $2) (snd $3) (snd $4)) }

CommandSigList :: { (Omni.Abs.BNFC'Position, Omni.Abs.CommandSigList) }
CommandSigList
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, Omni.Abs.NoCommandSigs Omni.Abs.BNFC'NoPosition) }
  | '=' ListCommandSig { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $2), Omni.Abs.SomeCommandSigs (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $2)) (snd $2)) }

CommandSig :: { (Omni.Abs.BNFC'Position, Omni.Abs.CommandSig) }
CommandSig
  : Name '(' ListNamedParam ')' ':' ValueType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $6), Omni.Abs.CommandSig (Omni.Abs.spanBNFC'Position (fst $1) (fst $6)) (snd $1) (snd $3) (snd $6)) }

NamedParam :: { (Omni.Abs.BNFC'Position, Omni.Abs.NamedParam) }
NamedParam
  : Name ':' ValueType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), Omni.Abs.NamedParam (Omni.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $3)) }

ListNamedParam :: { (Omni.Abs.BNFC'Position, [Omni.Abs.NamedParam]) }
ListNamedParam
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | NamedParam { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | NamedParam ',' ListNamedParam { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

ListCommandSig :: { (Omni.Abs.BNFC'Position, [Omni.Abs.CommandSig]) }
ListCommandSig
  : CommandSig { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | CommandSig '|' ListCommandSig { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

ValueType :: { (Omni.Abs.BNFC'Position, Omni.Abs.ValueType) }
ValueType
  : TypeName TypeArgList { (Omni.Abs.spanBNFC'Position (fst $1) (fst $2), Omni.Abs.TValueData (Omni.Abs.spanBNFC'Position (fst $1) (fst $2)) (snd $1) (snd $2)) }
  | '{' ComputationType '}' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.TValueComputation (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }
  | TypeVarName { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TValueParam (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | 'Unit' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)), Omni.Abs.TUnit (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)))) }
  | 'Int' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)), Omni.Abs.TInt (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)))) }

ListValueType :: { (Omni.Abs.BNFC'Position, [Omni.Abs.ValueType]) }
ListValueType
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | ValueType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | ValueType ',' ListValueType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

ValueTypeList :: { (Omni.Abs.BNFC'Position, Omni.Abs.ValueTypeList) }
ValueTypeList
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, Omni.Abs.NoValueTypes Omni.Abs.BNFC'NoPosition) }
  | '(' ListValueType ')' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.SomeValueTypes (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }

ComputationType :: { (Omni.Abs.BNFC'Position, Omni.Abs.ComputationType) }
ComputationType
  : '(' ListPortType ')' ':' PegType { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $5), Omni.Abs.TComputation (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $5)) (snd $2) (snd $5)) }

PortType :: { (Omni.Abs.BNFC'Position, Omni.Abs.PortType) }
PortType
  : ValueType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TPortNone (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | '<' AdjustmentType '>' ValueType { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4), Omni.Abs.TPortSome (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4)) (snd $2) (snd $4)) }

ListPortType :: { (Omni.Abs.BNFC'Position, [Omni.Abs.PortType]) }
ListPortType
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | PortType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | PortType ',' ListPortType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

PegType :: { (Omni.Abs.BNFC'Position, Omni.Abs.PegType) }
PegType
  : ValueType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TPegNone (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | '[' AbilityType ']' ValueType { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4), Omni.Abs.TPegSome (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4)) (snd $2) (snd $4)) }

TypeVariable :: { (Omni.Abs.BNFC'Position, Omni.Abs.TypeVariable) }
TypeVariable
  : TypeVarName { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TVar (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | '[' EffectVarName ']' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.TVarEffect (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }

ListTypeVariable :: { (Omni.Abs.BNFC'Position, [Omni.Abs.TypeVariable]) }
ListTypeVariable
  : TypeVariable { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | TypeVariable ',' ListTypeVariable { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

TypeVarList :: { (Omni.Abs.BNFC'Position, Omni.Abs.TypeVarList) }
TypeVarList
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, Omni.Abs.NoTypeVars Omni.Abs.BNFC'NoPosition) }
  | '(' ListTypeVariable ')' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.SomeTypeVars (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }

TypeArgument :: { (Omni.Abs.BNFC'Position, Omni.Abs.TypeArgument) }
TypeArgument
  : ValueType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TArgValue (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | '[' AbilityType ']' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.TArgAbility (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }

ListTypeArgument :: { (Omni.Abs.BNFC'Position, [Omni.Abs.TypeArgument]) }
ListTypeArgument
  : TypeArgument { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | TypeArgument ',' ListTypeArgument { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

TypeArgList :: { (Omni.Abs.BNFC'Position, Omni.Abs.TypeArgList) }
TypeArgList
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, Omni.Abs.NoTypeArgs Omni.Abs.BNFC'NoPosition) }
  | '(' ListTypeArgument ')' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.SomeTypeArgs (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }

InterfaceType :: { (Omni.Abs.BNFC'Position, Omni.Abs.InterfaceType) }
InterfaceType
  : TypeName TypeArgList { (Omni.Abs.spanBNFC'Position (fst $1) (fst $2), Omni.Abs.TInterface (Omni.Abs.spanBNFC'Position (fst $1) (fst $2)) (snd $1) (snd $2)) }

ListInterfaceType :: { (Omni.Abs.BNFC'Position, [Omni.Abs.InterfaceType]) }
ListInterfaceType
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | InterfaceType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | InterfaceType ',' ListInterfaceType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

AbilityType :: { (Omni.Abs.BNFC'Position, Omni.Abs.AbilityType) }
AbilityType
  : ListInterfaceType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TAbilityInterfaces (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | EffectVarName { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TAbilityEffectVar (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

AdjustmentType :: { (Omni.Abs.BNFC'Position, Omni.Abs.AdjustmentType) }
AdjustmentType
  : ListInterfaceType { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.TAdjustment (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }

Term2 :: { (Omni.Abs.BNFC'Position, Omni.Abs.Term) }
Term2
  : Name { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.EIdent (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | Integer { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.EIntLit (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | '(' ')' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $2)), Omni.Abs.EUnit (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $2)))) }
  | '(' Term ')' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), (snd $2)) }

Term1 :: { (Omni.Abs.BNFC'Position, Omni.Abs.Term) }
Term1
  : Term2 '(' ListTerm ')' { (Omni.Abs.spanBNFC'Position (fst $1) (uncurry Omni.Abs.BNFC'Position (tokenSpan $4)), Omni.Abs.EApplication (Omni.Abs.spanBNFC'Position (fst $1) (uncurry Omni.Abs.BNFC'Position (tokenSpan $4))) (snd $1) (snd $3)) }
  | Term1 InfixOpName Term2 { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), Omni.Abs.EInfixOp (Omni.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $2) (snd $3)) }
  | '{' ListComputationTerm '}' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.EThunk (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }
  | Term2 { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }

Term :: { (Omni.Abs.BNFC'Position, Omni.Abs.Term) }
Term
  : 'let' ListBinding 'in' Term { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4), Omni.Abs.EConLet (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (fst $4)) (snd $2) (snd $4)) }
  | Term1 { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (snd $1)) }

ListTerm :: { (Omni.Abs.BNFC'Position, [Omni.Abs.Term]) }
ListTerm
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | Term { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | Term ',' ListTerm { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

Binding :: { (Omni.Abs.BNFC'Position, Omni.Abs.Binding) }
Binding
  : Name '=' Term { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), Omni.Abs.Bind (Omni.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $3)) }
  | Name ':' ValueType '=' Term { (Omni.Abs.spanBNFC'Position (fst $1) (fst $5), Omni.Abs.BindAnnotated (Omni.Abs.spanBNFC'Position (fst $1) (fst $5)) (snd $1) (snd $3) (snd $5)) }

ListBinding :: { (Omni.Abs.BNFC'Position, [Omni.Abs.Binding]) }
ListBinding
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | Binding { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | Binding ',' ListBinding { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

ComputationTerm :: { (Omni.Abs.BNFC'Position, Omni.Abs.ComputationTerm) }
ComputationTerm
  : ListComputationPattern '->' Term { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), Omni.Abs.EComputation (Omni.Abs.spanBNFC'Position (fst $1) (fst $3)) (snd $1) (snd $3)) }

ListComputationTerm :: { (Omni.Abs.BNFC'Position, [Omni.Abs.ComputationTerm]) }
ListComputationTerm
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | ComputationTerm { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | ComputationTerm '|' ListComputationTerm { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

ComputationPattern :: { (Omni.Abs.BNFC'Position, Omni.Abs.ComputationPattern) }
ComputationPattern
  : ValuePattern { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), Omni.Abs.CompPatValue (Omni.Abs.spanBNFC'Position (fst $1) (fst $1)) (snd $1)) }
  | '<' Name ValuePatternList '->' Name '>' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $6)), Omni.Abs.CompPatRequest (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $6))) (snd $2) (snd $3) (snd $5)) }
  | '<' Name '>' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.CompPatCatchAll (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }

ListComputationPattern :: { (Omni.Abs.BNFC'Position, [Omni.Abs.ComputationPattern]) }
ListComputationPattern
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, []) }
  | ComputationPattern { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | ComputationPattern '|' ListComputationPattern { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

ValuePatternList :: { (Omni.Abs.BNFC'Position, Omni.Abs.ValuePatternList) }
ValuePatternList
  : {- empty -} { (Omni.Abs.BNFC'NoPosition, Omni.Abs.NoValuePatterns Omni.Abs.BNFC'NoPosition) }
  | '(' ListValuePattern ')' { (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3)), Omni.Abs.SomeValuePatterns (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan $1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan $3))) (snd $2)) }

ValuePattern :: { (Omni.Abs.BNFC'Position, Omni.Abs.ValuePattern) }
ValuePattern
  : Name ValuePatternList { (Omni.Abs.spanBNFC'Position (fst $1) (fst $2), Omni.Abs.ValPat (Omni.Abs.spanBNFC'Position (fst $1) (fst $2)) (snd $1) (snd $2)) }

ListValuePattern :: { (Omni.Abs.BNFC'Position, [Omni.Abs.ValuePattern]) }
ListValuePattern
  : ValuePattern { (Omni.Abs.spanBNFC'Position (fst $1) (fst $1), (:[]) (snd $1)) }
  | ValuePattern ',' ListValuePattern { (Omni.Abs.spanBNFC'Position (fst $1) (fst $3), (:) (snd $1) (snd $3)) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: Data.Text.Text -> [Token]
myLexer = tokens

-- Entrypoints

pModule :: [Token] -> Err Omni.Abs.Module
pModule = fmap snd . pModule_internal
}

