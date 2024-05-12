{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 ((Omni.Abs.BNFC'Position, Integer))
happyIn4 :: ((Omni.Abs.BNFC'Position, Integer)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ((Omni.Abs.BNFC'Position, Omni.Abs.UpperIdent))
happyIn5 :: ((Omni.Abs.BNFC'Position, Omni.Abs.UpperIdent)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ((Omni.Abs.BNFC'Position, Omni.Abs.LowerIdent))
happyIn6 :: ((Omni.Abs.BNFC'Position, Omni.Abs.LowerIdent)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ((Omni.Abs.BNFC'Position, Omni.Abs.InfixOpIdent))
happyIn7 :: ((Omni.Abs.BNFC'Position, Omni.Abs.InfixOpIdent)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 ((Omni.Abs.BNFC'Position, Omni.Abs.TypeName))
happyIn8 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TypeName)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ((Omni.Abs.BNFC'Position, Omni.Abs.ConstructorName))
happyIn9 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ConstructorName)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ((Omni.Abs.BNFC'Position, Omni.Abs.Name))
happyIn10 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Name)) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 ((Omni.Abs.BNFC'Position, Omni.Abs.TypeVarName))
happyIn11 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TypeVarName)) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 ((Omni.Abs.BNFC'Position, Omni.Abs.EffectVarName))
happyIn12 :: ((Omni.Abs.BNFC'Position, Omni.Abs.EffectVarName)) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 ((Omni.Abs.BNFC'Position, Omni.Abs.Module))
happyIn13 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Module)) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ((Omni.Abs.BNFC'Position, Omni.Abs.TopDef))
happyIn14 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TopDef)) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ((Omni.Abs.BNFC'Position, [Omni.Abs.TopDef]))
happyIn15 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.TopDef])) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ((Omni.Abs.BNFC'Position, Omni.Abs.FnDef))
happyIn16 :: ((Omni.Abs.BNFC'Position, Omni.Abs.FnDef)) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ((Omni.Abs.BNFC'Position, Omni.Abs.FnSig))
happyIn17 :: ((Omni.Abs.BNFC'Position, Omni.Abs.FnSig)) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ((Omni.Abs.BNFC'Position, Omni.Abs.NamedPort))
happyIn18 :: ((Omni.Abs.BNFC'Position, Omni.Abs.NamedPort)) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 ((Omni.Abs.BNFC'Position, [Omni.Abs.NamedPort]))
happyIn19 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.NamedPort])) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ((Omni.Abs.BNFC'Position, Omni.Abs.DataDef))
happyIn20 :: ((Omni.Abs.BNFC'Position, Omni.Abs.DataDef)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ((Omni.Abs.BNFC'Position, Omni.Abs.ConstructorList))
happyIn21 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ConstructorList)) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 ((Omni.Abs.BNFC'Position, Omni.Abs.Constructor))
happyIn22 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Constructor)) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ((Omni.Abs.BNFC'Position, [Omni.Abs.Constructor]))
happyIn23 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.Constructor])) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 ((Omni.Abs.BNFC'Position, Omni.Abs.InterfaceDef))
happyIn24 :: ((Omni.Abs.BNFC'Position, Omni.Abs.InterfaceDef)) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 ((Omni.Abs.BNFC'Position, Omni.Abs.CommandSigList))
happyIn25 :: ((Omni.Abs.BNFC'Position, Omni.Abs.CommandSigList)) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 ((Omni.Abs.BNFC'Position, Omni.Abs.CommandSig))
happyIn26 :: ((Omni.Abs.BNFC'Position, Omni.Abs.CommandSig)) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 ((Omni.Abs.BNFC'Position, Omni.Abs.NamedParam))
happyIn27 :: ((Omni.Abs.BNFC'Position, Omni.Abs.NamedParam)) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ((Omni.Abs.BNFC'Position, [Omni.Abs.NamedParam]))
happyIn28 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.NamedParam])) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 ((Omni.Abs.BNFC'Position, [Omni.Abs.CommandSig]))
happyIn29 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.CommandSig])) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 ((Omni.Abs.BNFC'Position, Omni.Abs.ValueType))
happyIn30 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ValueType)) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 ((Omni.Abs.BNFC'Position, [Omni.Abs.ValueType]))
happyIn31 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.ValueType])) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 ((Omni.Abs.BNFC'Position, Omni.Abs.ValueTypeList))
happyIn32 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ValueTypeList)) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ((Omni.Abs.BNFC'Position, Omni.Abs.ComputationType))
happyIn33 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ComputationType)) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 ((Omni.Abs.BNFC'Position, Omni.Abs.PortType))
happyIn34 :: ((Omni.Abs.BNFC'Position, Omni.Abs.PortType)) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 ((Omni.Abs.BNFC'Position, [Omni.Abs.PortType]))
happyIn35 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.PortType])) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 ((Omni.Abs.BNFC'Position, Omni.Abs.PegType))
happyIn36 :: ((Omni.Abs.BNFC'Position, Omni.Abs.PegType)) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 ((Omni.Abs.BNFC'Position, Omni.Abs.TypeVariable))
happyIn37 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TypeVariable)) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ((Omni.Abs.BNFC'Position, [Omni.Abs.TypeVariable]))
happyIn38 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.TypeVariable])) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 ((Omni.Abs.BNFC'Position, Omni.Abs.TypeVarList))
happyIn39 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TypeVarList)) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 ((Omni.Abs.BNFC'Position, Omni.Abs.TypeArgument))
happyIn40 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TypeArgument)) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 ((Omni.Abs.BNFC'Position, [Omni.Abs.TypeArgument]))
happyIn41 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.TypeArgument])) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 ((Omni.Abs.BNFC'Position, Omni.Abs.TypeArgList))
happyIn42 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TypeArgList)) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 ((Omni.Abs.BNFC'Position, Omni.Abs.InterfaceType))
happyIn43 :: ((Omni.Abs.BNFC'Position, Omni.Abs.InterfaceType)) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 ((Omni.Abs.BNFC'Position, [Omni.Abs.InterfaceType]))
happyIn44 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.InterfaceType])) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 ((Omni.Abs.BNFC'Position, Omni.Abs.AbilityType))
happyIn45 :: ((Omni.Abs.BNFC'Position, Omni.Abs.AbilityType)) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ((Omni.Abs.BNFC'Position, Omni.Abs.AdjustmentType))
happyIn46 :: ((Omni.Abs.BNFC'Position, Omni.Abs.AdjustmentType)) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 ((Omni.Abs.BNFC'Position, Omni.Abs.Term))
happyIn47 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Term)) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ((Omni.Abs.BNFC'Position, Omni.Abs.Term))
happyIn48 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Term)) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ((Omni.Abs.BNFC'Position, Omni.Abs.Term))
happyIn49 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Term)) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 ((Omni.Abs.BNFC'Position, [Omni.Abs.Term]))
happyIn50 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.Term])) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 ((Omni.Abs.BNFC'Position, Omni.Abs.Binding))
happyIn51 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Binding)) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 ((Omni.Abs.BNFC'Position, [Omni.Abs.Binding]))
happyIn52 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.Binding])) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 ((Omni.Abs.BNFC'Position, Omni.Abs.ComputationTerm))
happyIn53 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ComputationTerm)) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 ((Omni.Abs.BNFC'Position, [Omni.Abs.ComputationTerm]))
happyIn54 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.ComputationTerm])) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 ((Omni.Abs.BNFC'Position, Omni.Abs.ComputationPattern))
happyIn55 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ComputationPattern)) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ((Omni.Abs.BNFC'Position, [Omni.Abs.ComputationPattern]))
happyIn56 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.ComputationPattern])) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 ((Omni.Abs.BNFC'Position, Omni.Abs.ValuePatternList))
happyIn57 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ValuePatternList)) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 ((Omni.Abs.BNFC'Position, Omni.Abs.ValuePattern))
happyIn58 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ValuePattern)) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ((Omni.Abs.BNFC'Position, [Omni.Abs.ValuePattern]))
happyIn59 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.ValuePattern])) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x94\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x40\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x10\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x94\x03\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x90\x01\x31\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x03\x31\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x94\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x18\x10\x03\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x94\x03\x00\x00\x00\x00\x00\x00\x00\x10\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x31\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x40\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x19\x10\x03\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x10\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x80\x01\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x40\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x18\x10\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x10\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x10\x03\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x10\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x03\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x03\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pModule_internal","Integer","UpperIdent","LowerIdent","InfixOpIdent","TypeName","ConstructorName","Name","TypeVarName","EffectVarName","Module","TopDef","ListTopDef","FnDef","FnSig","NamedPort","ListNamedPort","DataDef","ConstructorList","Constructor","ListConstructor","InterfaceDef","CommandSigList","CommandSig","NamedParam","ListNamedParam","ListCommandSig","ValueType","ListValueType","ValueTypeList","ComputationType","PortType","ListPortType","PegType","TypeVariable","ListTypeVariable","TypeVarList","TypeArgument","ListTypeArgument","TypeArgList","InterfaceType","ListInterfaceType","AbilityType","AdjustmentType","Term2","Term1","Term","ListTerm","Binding","ListBinding","ComputationTerm","ListComputationTerm","ComputationPattern","ListComputationPattern","ValuePatternList","ValuePattern","ListValuePattern","'('","')'","','","'->'","':'","'<'","'='","'>'","'Int'","'Unit'","'['","']'","'data'","'in'","'interface'","'let'","'module'","'{'","'|'","'}'","L_integ","L_UpperIdent","L_LowerIdent","L_InfixOpIdent","%eof"]
        bit_start = st Prelude.* 84
        bit_end = (st Prelude.+ 1) Prelude.* 84
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..83]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xf0\xff\xf1\xff\x00\x00\xef\xff\xfd\xff\x2e\x00\x00\x00\x32\x00\x29\x01\x00\x00\x00\x00\x34\x00\x29\x01\x00\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x29\x00\x29\x00\x00\x00\x00\x00\x4a\x00\x4a\x00\x44\x01\x00\x00\x0b\x00\x4f\x00\x4c\x00\x60\x00\x00\x00\x00\x00\x62\x00\x54\x00\x00\x00\x3a\x01\x0b\x00\x79\x01\x66\x00\x04\x00\x76\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x7f\x00\x8c\x00\x80\x00\x00\x00\x77\x00\x95\x00\x85\x00\x8e\x00\x90\x00\xa5\x00\x00\x00\x0b\x00\xba\x00\xca\x00\xc4\x00\xdc\x00\x00\x00\x13\x00\x00\x00\x44\x01\xdb\x00\x0b\x00\x74\x01\xe1\x00\x00\x00\x00\x00\x00\x00\xd4\x00\x00\x00\x00\x00\xeb\x00\x00\x00\x88\x01\xec\x00\xef\x00\x00\x00\x00\x00\x44\x01\x0b\x00\x72\x01\x0d\x00\x44\x01\x3f\x00\x00\x00\x79\x01\x00\x00\x0b\x00\x00\x00\xf1\x00\xe0\x00\x00\x00\x00\x00\xee\x00\x00\x00\x04\x00\xfe\x00\xf6\x00\x00\x00\x0b\x00\x0b\x00\x00\x00\x00\x00\xf4\x00\x00\x00\x72\x01\x09\x01\x0b\x01\x00\x00\x00\x00\x00\x00\x0a\x01\x00\x00\x0d\x01\x00\x00\x00\x00\x00\x00\x44\x01\x00\x00\x00\x00\x0b\x00\xfd\x00\x74\x01\x0f\x01\x12\x01\x00\x00\x10\x01\x00\x00\x8b\x01\x00\x00\x16\x01\x38\x01\x0b\x00\x72\x01\x28\x01\x00\x00\x43\x01\x49\x01\x00\x00\x00\x00\x00\x00\x06\x01\x00\x00\x44\x01\x0b\x00\x00\x00\x0b\x00\x4a\x01\x4c\x01\x00\x00\x4d\x01\x50\x01\x53\x01\x00\x00\x58\x01\x0b\x00\x72\x01\x00\x00\x72\x01\x00\x00\x57\x01\x00\x00\x72\x01\x59\x01\x74\x01\x00\x00\x00\x00\x5c\x01\x00\x00\x8b\x01\x00\x00\x00\x00\x00\x00\x8e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x69\x01\x00\x00\x00\x00\x00\x00\x6b\x01\x00\x00\x00\x00\x00\x00\x56\x01\x00\x00\x00\x00\x00\x00\x63\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\x00\xd8\x00\x00\x00\x00\x00\x51\x01\x5d\x01\x5b\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x01\x00\x00\x5e\x00\x18\x00\x0a\x00\x76\x01\x97\x00\x6c\x01\x00\x00\xe2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x66\x01\x67\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\x00\x00\x00\x25\x00\x00\x00\xae\x00\xf5\x00\x5f\x01\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x6f\x01\x00\x00\xc1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\x1e\x00\x04\x01\x6e\x01\x68\x00\x16\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x00\x00\x00\x00\x00\x00\x00\x2e\x01\x2b\x01\x00\x00\x00\x00\x70\x01\x00\x00\xfc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x78\x00\x00\x00\xcf\x00\x68\x01\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x7d\x00\x1b\x01\x8b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\xe5\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x01\x1f\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x23\x01\x00\x00\xd6\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\x00\x00\x00\x00\xc5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x01\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\xf0\xff\xf8\xff\xf7\xff\x00\x00\xf0\xff\xf4\xff\xf3\xff\x00\x00\xf2\xff\xf1\xff\x00\x00\x00\x00\xfc\xff\xfa\xff\xc2\xff\xc2\xff\x00\x00\xef\xff\xeb\xff\x00\x00\xea\xff\x00\x00\xb2\xff\xb3\xff\xac\xff\xaa\xff\xee\xff\x00\x00\xa5\xff\x9b\xff\xe7\xff\x00\x00\xe1\xff\xe2\xff\x00\x00\xf6\xff\xc6\xff\xc4\xff\x00\x00\x00\x00\xe8\xff\x00\x00\x98\xff\xa0\xff\x00\x00\x9a\xff\x00\x00\x9e\xff\x00\x00\x00\x00\xa4\xff\x00\x00\x00\x00\xb1\xff\x00\x00\xfb\xff\xa9\xff\x00\x00\xeb\xff\x00\x00\xbc\xff\xd6\xff\xcd\xff\xec\xff\xb9\xff\xd4\xff\xd5\xff\x00\x00\xe9\xff\x00\x00\xa8\xff\x00\x00\xae\xff\xb0\xff\x00\x00\xa5\xff\x00\x00\x98\xff\x00\x00\x9b\xff\xad\xff\x9b\xff\x96\xff\x00\x00\xf9\xff\xd0\xff\xe4\xff\xe6\xff\xf5\xff\x00\x00\xc1\xff\x00\x00\x00\x00\xda\xff\xe0\xff\x00\x00\xdd\xff\xc3\xff\xc5\xff\x00\x00\xe5\xff\xd3\xff\x95\xff\x00\x00\x9f\xff\x99\xff\xa2\xff\x00\x00\x9c\xff\x00\x00\xa3\xff\xab\xff\xaf\xff\xa9\xff\xc8\xff\xed\xff\xb9\xff\x00\x00\xcb\xff\xbc\xff\xb8\xff\xb4\xff\x00\x00\xd8\xff\x00\x00\xc0\xff\xbe\xff\x00\x00\xb9\xff\x00\x00\xb9\xff\xba\xff\xca\xff\x00\x00\xd7\xff\xb5\xff\xb6\xff\x00\x00\xa7\xff\x00\x00\x00\x00\x97\xff\x00\x00\xd2\xff\x00\x00\xe3\xff\x00\x00\xdc\xff\x00\x00\xd9\xff\x00\x00\xdd\xff\x00\x00\xcf\xff\xd3\xff\x94\xff\x00\x00\xa6\xff\x00\x00\x00\x00\xcb\xff\xb7\xff\xcc\xff\x00\x00\xbb\xff\x00\x00\xbd\xff\xbf\xff\xc9\xff\x00\x00\xc7\xff\x9d\xff\xd1\xff\xde\xff\xdb\xff\x00\x00\xdf\xff\xce\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x11\x00\x01\x00\x02\x00\x01\x00\x02\x00\x15\x00\x06\x00\x19\x00\x06\x00\x01\x00\x01\x00\x02\x00\x04\x00\x01\x00\x0b\x00\x06\x00\x01\x00\x02\x00\x16\x00\x01\x00\x08\x00\x06\x00\x01\x00\x02\x00\x01\x00\x02\x00\x17\x00\x06\x00\x02\x00\x06\x00\x01\x00\x02\x00\x16\x00\x17\x00\x08\x00\x06\x00\x00\x00\x01\x00\x02\x00\x15\x00\x16\x00\x17\x00\x06\x00\x00\x00\x01\x00\x02\x00\x01\x00\x27\x00\x28\x00\x06\x00\x2a\x00\x02\x00\x01\x00\x07\x00\x36\x00\x37\x00\x36\x00\x37\x00\x31\x00\x32\x00\x33\x00\x34\x00\x16\x00\x36\x00\x31\x00\x32\x00\x33\x00\x34\x00\x06\x00\x36\x00\x2f\x00\x30\x00\x33\x00\x34\x00\x01\x00\x36\x00\x2f\x00\x30\x00\x03\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x05\x00\x16\x00\x17\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x06\x00\x02\x00\x01\x00\x06\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x06\x00\x18\x00\x07\x00\x06\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x06\x00\x01\x00\x02\x00\x06\x00\x01\x00\x02\x00\x06\x00\x04\x00\x07\x00\x01\x00\x02\x00\x08\x00\x04\x00\x03\x00\x0e\x00\x0f\x00\x08\x00\x2b\x00\x2c\x00\x2d\x00\x2b\x00\x2c\x00\x2d\x00\x01\x00\x16\x00\x02\x00\x04\x00\x2b\x00\x2c\x00\x2d\x00\x2b\x00\x2c\x00\x2d\x00\x01\x00\x17\x00\x13\x00\x02\x00\x2b\x00\x2c\x00\x2d\x00\x2b\x00\x07\x00\x27\x00\x28\x00\x29\x00\x14\x00\x13\x00\x27\x00\x28\x00\x29\x00\x01\x00\x02\x00\x04\x00\x04\x00\x01\x00\x02\x00\x07\x00\x04\x00\x01\x00\x02\x00\x07\x00\x27\x00\x28\x00\x06\x00\x02\x00\x01\x00\x02\x00\x21\x00\x22\x00\x07\x00\x06\x00\x0e\x00\x0f\x00\x01\x00\x05\x00\x1a\x00\x04\x00\x01\x00\x02\x00\x1a\x00\x04\x00\x01\x00\x02\x00\x07\x00\x04\x00\x24\x00\x25\x00\x07\x00\x03\x00\x24\x00\x25\x00\x01\x00\x02\x00\x0e\x00\x04\x00\x21\x00\x22\x00\x07\x00\x01\x00\x02\x00\x01\x00\x04\x00\x1a\x00\x04\x00\x07\x00\x02\x00\x1a\x00\x05\x00\x20\x00\x01\x00\x01\x00\x02\x00\x20\x00\x01\x00\x02\x00\x06\x00\x1a\x00\x16\x00\x06\x00\x01\x00\x1e\x00\x1f\x00\x03\x00\x1a\x00\x02\x00\x01\x00\x13\x00\x1e\x00\x1f\x00\x01\x00\x02\x00\x16\x00\x04\x00\x0c\x00\x19\x00\x07\x00\x01\x00\x02\x00\x01\x00\x04\x00\x01\x00\x02\x00\x07\x00\x04\x00\x01\x00\x02\x00\x07\x00\x04\x00\x13\x00\x16\x00\x07\x00\x03\x00\x02\x00\x04\x00\x1a\x00\x01\x00\x14\x00\x0c\x00\x1e\x00\x07\x00\x03\x00\x1a\x00\x1b\x00\x08\x00\x03\x00\x1a\x00\x1b\x00\x01\x00\x02\x00\x1a\x00\x04\x00\x01\x00\x02\x00\x07\x00\x04\x00\x01\x00\x02\x00\x07\x00\x04\x00\x01\x00\x02\x00\x07\x00\x04\x00\x01\x00\x02\x00\x07\x00\x01\x00\x02\x00\x06\x00\x01\x00\x02\x00\x06\x00\x1a\x00\x0d\x00\x06\x00\x0f\x00\x1a\x00\x02\x00\x01\x00\x02\x00\x1a\x00\x16\x00\x16\x00\x17\x00\x1a\x00\x17\x00\x18\x00\x16\x00\x01\x00\x03\x00\x19\x00\x17\x00\x18\x00\x10\x00\x02\x00\x12\x00\x03\x00\x02\x00\x15\x00\x16\x00\x17\x00\x05\x00\x03\x00\x10\x00\x02\x00\x12\x00\x01\x00\x02\x00\x15\x00\x16\x00\x17\x00\x06\x00\x05\x00\x05\x00\x08\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x01\x00\x02\x00\x10\x00\x01\x00\x0c\x00\x06\x00\x14\x00\x05\x00\x01\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x01\x00\x09\x00\x10\x00\x23\x00\x05\x00\x03\x00\x14\x00\x12\x00\x13\x00\x06\x00\x09\x00\x0a\x00\x09\x00\x0a\x00\x06\x00\x23\x00\x15\x00\x12\x00\x13\x00\x12\x00\x26\x00\x12\x00\x11\x00\x16\x00\x17\x00\x16\x00\x17\x00\x1d\x00\x14\x00\x26\x00\x16\x00\x17\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x12\x00\x1c\x00\x35\x00\x12\x00\x16\x00\x17\x00\x12\x00\x16\x00\x17\x00\x35\x00\x16\x00\x17\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x05\x00\x09\x00\x0a\x00\x09\x00\x0a\x00\x03\x00\x32\x00\xff\xff\x32\x00\x15\x00\x09\x00\x0a\x00\x7f\x00\x5c\x00\x30\x00\x32\x00\x09\x00\x0a\x00\x07\x00\x24\x00\x75\x00\x32\x00\x09\x00\x0a\x00\x09\x00\x0a\x00\x15\x00\x32\x00\x60\x00\x39\x00\x09\x00\x0a\x00\x07\x00\x15\x00\x61\x00\x39\x00\x1e\x00\x09\x00\x0a\x00\x03\x00\x07\x00\x15\x00\x1f\x00\x1e\x00\x09\x00\x0a\x00\x08\x00\x80\x00\x81\x00\x1f\x00\x82\x00\x09\x00\x1b\x00\x19\x00\x6e\x00\x6f\x00\x6e\x00\xa3\x00\x33\x00\x34\x00\x35\x00\x36\x00\x07\x00\x37\x00\x33\x00\x70\x00\x35\x00\x36\x00\x39\x00\x37\x00\x3a\x00\x3b\x00\x35\x00\x71\x00\x28\x00\x37\x00\x3a\x00\x76\x00\x43\x00\x20\x00\x21\x00\x4e\x00\x4f\x00\x44\x00\x07\x00\x15\x00\x20\x00\x21\x00\x4e\x00\x92\x00\x1e\x00\x09\x00\x0a\x00\x1e\x00\x09\x00\x0a\x00\x1f\x00\x42\x00\x41\x00\x1f\x00\x1e\x00\x09\x00\x0a\x00\x1e\x00\x09\x00\x0a\x00\x1f\x00\x40\x00\x32\x00\x1f\x00\x1e\x00\x09\x00\x0a\x00\x1e\x00\x09\x00\x0a\x00\x1f\x00\x09\x00\x0a\x00\x1f\x00\x15\x00\x60\x00\x1b\x00\x7f\x00\x2b\x00\x15\x00\x60\x00\x8f\x00\x7f\x00\x64\x00\x1c\x00\x1d\x00\x8f\x00\x20\x00\x21\x00\x22\x00\x20\x00\x21\x00\x3c\x00\x15\x00\x07\x00\x63\x00\x7f\x00\x20\x00\x21\x00\x77\x00\x20\x00\x21\x00\x72\x00\x5c\x00\x15\x00\x5a\x00\x2b\x00\x20\x00\x21\x00\xa5\x00\x50\x00\x2c\x00\x80\x00\x90\x00\x91\x00\x59\x00\x58\x00\x80\x00\x90\x00\xab\x00\x15\x00\x2b\x00\x57\x00\x44\x00\x15\x00\x2b\x00\x45\x00\x44\x00\x09\x00\x0a\x00\x45\x00\x80\x00\xa9\x00\x1b\x00\x2b\x00\x09\x00\x0a\x00\x2d\x00\x2e\x00\x2c\x00\x55\x00\x1c\x00\x4c\x00\x15\x00\x55\x00\x85\x00\x17\x00\x15\x00\x2b\x00\x85\x00\x44\x00\x15\x00\x2b\x00\x45\x00\x44\x00\x86\x00\x87\x00\x45\x00\x54\x00\x86\x00\xae\x00\x15\x00\x2b\x00\x53\x00\x44\x00\x2d\x00\x69\x00\x45\x00\x15\x00\x2b\x00\x15\x00\x44\x00\x7a\x00\x16\x00\x45\x00\x52\x00\x7a\x00\x4e\x00\x7b\x00\x85\x00\x09\x00\x0a\x00\xb9\x00\x09\x00\x0a\x00\x64\x00\x46\x00\x07\x00\xa4\x00\x7f\x00\x8c\x00\x8d\x00\x7a\x00\x46\x00\x79\x00\x6e\x00\x6c\x00\x8c\x00\xb0\x00\x15\x00\x2b\x00\x65\x00\x44\x00\x6b\x00\x66\x00\x45\x00\x15\x00\x2b\x00\x69\x00\x44\x00\x15\x00\x2b\x00\x45\x00\x44\x00\x15\x00\x2b\x00\x45\x00\x44\x00\x68\x00\x07\x00\x45\x00\x97\x00\x96\x00\x95\x00\x46\x00\x85\x00\x8f\x00\xa7\x00\x47\x00\x94\x00\x8b\x00\x97\x00\x98\x00\x8a\x00\xae\x00\x97\x00\xb4\x00\x15\x00\x2b\x00\x75\x00\x44\x00\x15\x00\x2b\x00\x45\x00\x44\x00\x15\x00\x2b\x00\x45\x00\x44\x00\x15\x00\x2b\x00\x45\x00\x44\x00\x09\x00\x0a\x00\x45\x00\x09\x00\x0a\x00\x9a\x00\x09\x00\x0a\x00\x64\x00\xaa\x00\x13\x00\x9a\x00\x14\x00\xb5\x00\xad\x00\x24\x00\x3e\x00\xb2\x00\x07\x00\x07\x00\x15\x00\xb8\x00\x9b\x00\x9c\x00\x65\x00\x24\x00\xa9\x00\x9d\x00\x9b\x00\xb6\x00\x25\x00\xa8\x00\x26\x00\xa3\x00\xa2\x00\x03\x00\x07\x00\x15\x00\xa1\x00\xa0\x00\x25\x00\x9f\x00\x26\x00\x09\x00\x0a\x00\x03\x00\x07\x00\x15\x00\x0b\x00\xb8\x00\xb2\x00\xb4\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x09\x00\x0a\x00\x10\x00\x5c\x00\xb0\x00\x0b\x00\x11\x00\x5d\x00\x05\x00\x0c\x00\x19\x00\x0e\x00\x0f\x00\x5c\x00\x03\x00\x10\x00\x28\x00\x5d\x00\x3e\x00\x11\x00\x5e\x00\x5f\x00\x49\x00\x4a\x00\x4b\x00\x4a\x00\x4b\x00\x39\x00\x26\x00\x29\x00\x5e\x00\x99\x00\x4c\x00\x83\x00\x4c\x00\x30\x00\x07\x00\x15\x00\x07\x00\x15\x00\x7d\x00\xa1\xff\x8b\x00\x07\x00\x15\x00\x4a\x00\x4b\x00\x7d\x00\x4a\x00\x4b\x00\x89\x00\x4a\x00\x4b\x00\x7d\x00\x4c\x00\x6c\x00\x5a\x00\x4c\x00\x07\x00\x15\x00\x4c\x00\x07\x00\x15\x00\x73\x00\x07\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 107) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107)
	]

happy_n_terms = 26 :: Prelude.Int
happy_n_nonterms = 56 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1), (read (Data.Text.unpack (tokenText happy_var_1))) :: Integer)
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1), Omni.Abs.UpperIdent (tokenText happy_var_1))
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1), Omni.Abs.LowerIdent (tokenText happy_var_1))
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1), Omni.Abs.InfixOpIdent (tokenText happy_var_1))
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn8
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TypeName (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn9
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.ConstructorName (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn10
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.UpperName (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_8 = happySpecReduce_1  6# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn10
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.LowerName (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_9 = happySpecReduce_1  7# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn11
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TypeVarName (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_10 = happySpecReduce_1  8# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn12
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.EffectVarName (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_11 = happyReduce 5# 9# happyReduction_11
happyReduction_11 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	case happyOut15 happy_x_5 of { (HappyWrap15 happy_var_5) -> 
	happyIn13
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_5), Omni.Abs.Module (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_5)) (snd happy_var_2) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_12 = happySpecReduce_1  10# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn14
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TopFnDef (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_13 = happySpecReduce_1  10# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn14
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TopDataDef (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_14 = happySpecReduce_1  10# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn14
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TopInterfaceDef (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_15 = happySpecReduce_0  11# happyReduction_15
happyReduction_15  =  happyIn15
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_16 = happySpecReduce_2  11# happyReduction_16
happyReduction_16 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn15
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_17 = happySpecReduce_3  12# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn16
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Omni.Abs.FnDef (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_18 = happyReduce 6# 13# happyReduction_18
happyReduction_18 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	case happyOut36 happy_x_6 of { (HappyWrap36 happy_var_6) -> 
	happyIn17
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_6), Omni.Abs.FnSig (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_6)) (snd happy_var_1) (snd happy_var_3) (snd happy_var_6))
	) `HappyStk` happyRest}}}

happyReduce_19 = happySpecReduce_3  14# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	happyIn18
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Omni.Abs.NamedPort (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_20 = happySpecReduce_0  15# happyReduction_20
happyReduction_20  =  happyIn19
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_21 = happySpecReduce_1  15# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn19
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_22 = happySpecReduce_3  15# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn19
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_23 = happyReduce 4# 16# happyReduction_23
happyReduction_23 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
	case happyOut39 happy_x_3 of { (HappyWrap39 happy_var_3) -> 
	case happyOut21 happy_x_4 of { (HappyWrap21 happy_var_4) -> 
	happyIn20
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Omni.Abs.DataDef (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_3) (snd happy_var_4))
	) `HappyStk` happyRest}}}}

happyReduce_24 = happySpecReduce_0  17# happyReduction_24
happyReduction_24  =  happyIn21
		 ((Omni.Abs.BNFC'NoPosition, Omni.Abs.NoConstructors Omni.Abs.BNFC'NoPosition)
	)

happyReduce_25 = happySpecReduce_2  17# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
	happyIn21
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_2), Omni.Abs.SomeConstructors (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_2)) (snd happy_var_2))
	)}}

happyReduce_26 = happySpecReduce_2  18# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	happyIn22
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), Omni.Abs.Constructor (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2)) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_27 = happySpecReduce_1  19# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn23
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_28 = happySpecReduce_3  19# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	happyIn23
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_29 = happyReduce 4# 20# happyReduction_29
happyReduction_29 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
	case happyOut39 happy_x_3 of { (HappyWrap39 happy_var_3) -> 
	case happyOut25 happy_x_4 of { (HappyWrap25 happy_var_4) -> 
	happyIn24
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Omni.Abs.InterfaceDef (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_3) (snd happy_var_4))
	) `HappyStk` happyRest}}}}

happyReduce_30 = happySpecReduce_0  21# happyReduction_30
happyReduction_30  =  happyIn25
		 ((Omni.Abs.BNFC'NoPosition, Omni.Abs.NoCommandSigs Omni.Abs.BNFC'NoPosition)
	)

happyReduce_31 = happySpecReduce_2  21# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn25
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_2), Omni.Abs.SomeCommandSigs (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_2)) (snd happy_var_2))
	)}}

happyReduce_32 = happyReduce 6# 22# happyReduction_32
happyReduction_32 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	case happyOut30 happy_x_6 of { (HappyWrap30 happy_var_6) -> 
	happyIn26
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_6), Omni.Abs.CommandSig (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_6)) (snd happy_var_1) (snd happy_var_3) (snd happy_var_6))
	) `HappyStk` happyRest}}}

happyReduce_33 = happySpecReduce_3  23# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn27
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Omni.Abs.NamedParam (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_34 = happySpecReduce_0  24# happyReduction_34
happyReduction_34  =  happyIn28
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_35 = happySpecReduce_1  24# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn28
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_36 = happySpecReduce_3  24# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn28
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_37 = happySpecReduce_1  25# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn29
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_38 = happySpecReduce_3  25# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn29
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_39 = happySpecReduce_2  26# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	happyIn30
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), Omni.Abs.TValueData (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2)) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_40 = happySpecReduce_3  26# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.TValueComputation (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_41 = happySpecReduce_1  26# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn30
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TValueParam (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_42 = happySpecReduce_1  26# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)), Omni.Abs.TUnit (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1))))
	)}

happyReduce_43 = happySpecReduce_1  26# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)), Omni.Abs.TInt (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1))))
	)}

happyReduce_44 = happySpecReduce_0  27# happyReduction_44
happyReduction_44  =  happyIn31
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_45 = happySpecReduce_1  27# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn31
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_46 = happySpecReduce_3  27# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	case happyOut31 happy_x_3 of { (HappyWrap31 happy_var_3) -> 
	happyIn31
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_47 = happySpecReduce_0  28# happyReduction_47
happyReduction_47  =  happyIn32
		 ((Omni.Abs.BNFC'NoPosition, Omni.Abs.NoValueTypes Omni.Abs.BNFC'NoPosition)
	)

happyReduce_48 = happySpecReduce_3  28# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn32
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.SomeValueTypes (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_49 = happyReduce 5# 29# happyReduction_49
happyReduction_49 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	case happyOut36 happy_x_5 of { (HappyWrap36 happy_var_5) -> 
	happyIn33
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_5), Omni.Abs.TComputation (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_5)) (snd happy_var_2) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_50 = happySpecReduce_1  30# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn34
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TPortNone (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_51 = happyReduce 4# 30# happyReduction_51
happyReduction_51 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	case happyOut30 happy_x_4 of { (HappyWrap30 happy_var_4) -> 
	happyIn34
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Omni.Abs.TPortSome (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_52 = happySpecReduce_0  31# happyReduction_52
happyReduction_52  =  happyIn35
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_53 = happySpecReduce_1  31# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn35
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_54 = happySpecReduce_3  31# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut35 happy_x_3 of { (HappyWrap35 happy_var_3) -> 
	happyIn35
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_55 = happySpecReduce_1  32# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn36
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TPegNone (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_56 = happyReduce 4# 32# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	case happyOut30 happy_x_4 of { (HappyWrap30 happy_var_4) -> 
	happyIn36
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Omni.Abs.TPegSome (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_57 = happySpecReduce_1  33# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn37
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TVar (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_58 = happySpecReduce_3  33# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn37
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.TVarEffect (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_59 = happySpecReduce_1  34# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	happyIn38
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_60 = happySpecReduce_3  34# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn38
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_61 = happySpecReduce_0  35# happyReduction_61
happyReduction_61  =  happyIn39
		 ((Omni.Abs.BNFC'NoPosition, Omni.Abs.NoTypeVars Omni.Abs.BNFC'NoPosition)
	)

happyReduce_62 = happySpecReduce_3  35# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn39
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.SomeTypeVars (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_63 = happySpecReduce_1  36# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn40
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TArgValue (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_64 = happySpecReduce_3  36# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn40
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.TArgAbility (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_65 = happySpecReduce_1  37# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn41
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_66 = happySpecReduce_3  37# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut41 happy_x_3 of { (HappyWrap41 happy_var_3) -> 
	happyIn41
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_67 = happySpecReduce_0  38# happyReduction_67
happyReduction_67  =  happyIn42
		 ((Omni.Abs.BNFC'NoPosition, Omni.Abs.NoTypeArgs Omni.Abs.BNFC'NoPosition)
	)

happyReduce_68 = happySpecReduce_3  38# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_2 of { (HappyWrap41 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn42
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.SomeTypeArgs (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_69 = happySpecReduce_2  39# happyReduction_69
happyReduction_69 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	happyIn43
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), Omni.Abs.TInterface (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2)) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_70 = happySpecReduce_0  40# happyReduction_70
happyReduction_70  =  happyIn44
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_71 = happySpecReduce_1  40# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn44
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_72 = happySpecReduce_3  40# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	happyIn44
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_73 = happySpecReduce_1  41# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn45
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TAbilityInterfaces (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_74 = happySpecReduce_1  41# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn45
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TAbilityEffectVar (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_75 = happySpecReduce_1  42# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn46
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TAdjustment (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_76 = happySpecReduce_1  43# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	happyIn47
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.EIdent (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_77 = happySpecReduce_1  43# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn47
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.EIntLit (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_78 = happySpecReduce_2  43# happyReduction_78
happyReduction_78 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn47
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_2)), Omni.Abs.EUnit (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_2))))
	)}}

happyReduce_79 = happySpecReduce_3  43# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn47
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), (snd happy_var_2))
	)}}}

happyReduce_80 = happyReduce 4# 44# happyReduction_80
happyReduction_80 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn48
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_4)), Omni.Abs.EApplication (Omni.Abs.spanBNFC'Position (fst happy_var_1) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_4))) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_81 = happySpecReduce_3  44# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	case happyOut47 happy_x_3 of { (HappyWrap47 happy_var_3) -> 
	happyIn48
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Omni.Abs.EInfixOp (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_82 = happySpecReduce_3  44# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_2 of { (HappyWrap54 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn48
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.EConSuspendedCom (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_83 = happySpecReduce_1  44# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn48
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
	)}

happyReduce_84 = happyReduce 4# 45# happyReduction_84
happyReduction_84 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	happyIn49
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Omni.Abs.EConLet (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_85 = happySpecReduce_1  45# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn49
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
	)}

happyReduce_86 = happySpecReduce_0  46# happyReduction_86
happyReduction_86  =  happyIn50
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_87 = happySpecReduce_1  46# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn50
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_88 = happySpecReduce_3  46# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn50
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_89 = happyReduce 5# 47# happyReduction_89
happyReduction_89 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	case happyOut49 happy_x_5 of { (HappyWrap49 happy_var_5) -> 
	happyIn51
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_5), Omni.Abs.BindAnnotated (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_5)) (snd happy_var_1) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_90 = happySpecReduce_0  48# happyReduction_90
happyReduction_90  =  happyIn52
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_91 = happySpecReduce_1  48# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn52
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_92 = happySpecReduce_3  48# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn52
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_93 = happySpecReduce_3  49# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn53
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Omni.Abs.EComputation (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_94 = happySpecReduce_0  50# happyReduction_94
happyReduction_94  =  happyIn54
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_95 = happySpecReduce_1  50# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn54
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_96 = happySpecReduce_3  50# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn54
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_97 = happySpecReduce_1  51# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn55
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.CompPatValue (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_98 = happyReduce 6# 51# happyReduction_98
happyReduction_98 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut10 happy_x_5 of { (HappyWrap10 happy_var_5) -> 
	case happyOutTok happy_x_6 of { happy_var_6 -> 
	happyIn55
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_6)), Omni.Abs.CompPatRequest (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_6))) (snd happy_var_2) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}}}

happyReduce_99 = happySpecReduce_3  51# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn55
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.CompPatCatchAll (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_100 = happySpecReduce_0  52# happyReduction_100
happyReduction_100  =  happyIn56
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_101 = happySpecReduce_1  52# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	happyIn56
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_102 = happySpecReduce_3  52# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn56
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_103 = happySpecReduce_0  53# happyReduction_103
happyReduction_103  =  happyIn57
		 ((Omni.Abs.BNFC'NoPosition, Omni.Abs.NoValuePatterns Omni.Abs.BNFC'NoPosition)
	)

happyReduce_104 = happySpecReduce_3  53# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_2 of { (HappyWrap59 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn57
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.SomeValuePatterns (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_105 = happySpecReduce_2  54# happyReduction_105
happyReduction_105 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn58
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), Omni.Abs.ValPat (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2)) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_106 = happySpecReduce_1  55# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn59
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_107 = happySpecReduce_3  55# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn59
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyNewToken action sts stk [] =
	happyDoAction 25# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ _ (TS _ 1) -> cont 1#;
	PT _ _ (TS _ 2) -> cont 2#;
	PT _ _ (TS _ 3) -> cont 3#;
	PT _ _ (TS _ 4) -> cont 4#;
	PT _ _ (TS _ 5) -> cont 5#;
	PT _ _ (TS _ 6) -> cont 6#;
	PT _ _ (TS _ 7) -> cont 7#;
	PT _ _ (TS _ 8) -> cont 8#;
	PT _ _ (TS _ 9) -> cont 9#;
	PT _ _ (TS _ 10) -> cont 10#;
	PT _ _ (TS _ 11) -> cont 11#;
	PT _ _ (TS _ 12) -> cont 12#;
	PT _ _ (TS _ 13) -> cont 13#;
	PT _ _ (TS _ 14) -> cont 14#;
	PT _ _ (TS _ 15) -> cont 15#;
	PT _ _ (TS _ 16) -> cont 16#;
	PT _ _ (TS _ 17) -> cont 17#;
	PT _ _ (TS _ 18) -> cont 18#;
	PT _ _ (TS _ 19) -> cont 19#;
	PT _ _ (TS _ 20) -> cont 20#;
	PT _ _ (TI _) -> cont 21#;
	PT _ _ (T_UpperIdent _) -> cont 22#;
	PT _ _ (T_LowerIdent _) -> cont 23#;
	PT _ _ (T_InfixOpIdent _) -> cont 24#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 25# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pModule_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap13 x') = happyOut13 x} in x'))

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
