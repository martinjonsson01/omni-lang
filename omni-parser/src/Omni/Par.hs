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
newtype HappyWrap4 = HappyWrap4 ((Omni.Abs.BNFC'Position, Omni.Abs.Ident))
happyIn4 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Ident)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ((Omni.Abs.BNFC'Position, Integer))
happyIn5 :: ((Omni.Abs.BNFC'Position, Integer)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ((Omni.Abs.BNFC'Position, Omni.Abs.InfixOpIdent))
happyIn6 :: ((Omni.Abs.BNFC'Position, Omni.Abs.InfixOpIdent)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ((Omni.Abs.BNFC'Position, Omni.Abs.Module))
happyIn7 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Module)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 ((Omni.Abs.BNFC'Position, Omni.Abs.TopDef))
happyIn8 :: ((Omni.Abs.BNFC'Position, Omni.Abs.TopDef)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ((Omni.Abs.BNFC'Position, [Omni.Abs.TopDef]))
happyIn9 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.TopDef])) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ((Omni.Abs.BNFC'Position, Omni.Abs.ParamList))
happyIn10 :: ((Omni.Abs.BNFC'Position, Omni.Abs.ParamList)) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 ((Omni.Abs.BNFC'Position, Omni.Abs.Param))
happyIn11 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Param)) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 ((Omni.Abs.BNFC'Position, [Omni.Abs.Param]))
happyIn12 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.Param])) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 ((Omni.Abs.BNFC'Position, Omni.Abs.Type))
happyIn13 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Type)) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ((Omni.Abs.BNFC'Position, [Omni.Abs.Type]))
happyIn14 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.Type])) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ((Omni.Abs.BNFC'Position, Omni.Abs.Exp))
happyIn15 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Exp)) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ((Omni.Abs.BNFC'Position, Omni.Abs.Exp))
happyIn16 :: ((Omni.Abs.BNFC'Position, Omni.Abs.Exp)) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ((Omni.Abs.BNFC'Position, [Omni.Abs.Exp]))
happyIn17 :: ((Omni.Abs.BNFC'Position, [Omni.Abs.Exp])) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x01\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x08\x00\x00\x00\x80\x00\x00\x10\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x20\x00\x00\x80\x00\x00\x00\x04\x00\x00\x40\x00\x00\x00\x06\x02\x00\x00\x00\x00\x00\x00\x02\x00\x00\x03\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x30\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x04\x00\x00\x60\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x08\x00\xc0\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x0c\x04\x00\x00\x00\x00\x00\x30\x10\x00\x00\x0a\x08\x00\xc0\xc0\x00\x00\x18\x18\x00\x00\x00\x00\x00\x20\x81\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x18\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pModule_internal","Ident","Integer","InfixOpIdent","Module","TopDef","ListTopDef","ParamList","Param","ListParam","Type","ListType","Exp1","Exp","ListExp","'('","'()'","')'","','","'->'","':'","'='","'module'","L_Ident","L_integ","L_InfixOpIdent","%eof"]
        bit_start = st Prelude.* 29
        bit_end = (st Prelude.+ 1) Prelude.* 29
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..28]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xfb\xff\xfd\xff\x00\x00\xff\xff\x15\x00\x40\x00\x25\x00\x1f\x00\x31\x00\x00\x00\x00\x00\x3d\x00\x3b\x00\x42\x00\x43\x00\x46\x00\x2e\x00\x00\x00\x44\x00\x2e\x00\x00\x00\x00\x00\x45\x00\x2e\x00\x00\x00\x00\x00\x48\x00\x47\x00\x29\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x29\x00\x00\x00\x00\x00\x4a\x00\x2e\x00\x00\x00\x2e\x00\x17\x00\x29\x00\x29\x00\x00\x00\x18\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x4e\x00\x00\x00\x00\x00\x00\x00\x52\x00\x00\x00\x39\x00\x4f\x00\x41\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x38\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x09\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x2d\x00\x4b\x00\x16\x00\x01\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf7\xff\xf9\xff\xfb\xff\xf8\xff\x00\x00\xf4\xff\x00\x00\xf3\xff\x00\x00\x00\x00\xef\xff\x00\x00\xee\xff\xf0\xff\xf6\xff\xf4\xff\x00\x00\xf5\xff\xf2\xff\xed\xff\x00\x00\x00\x00\xeb\xff\xea\xff\xe5\xff\xfa\xff\x00\x00\xe9\xff\xfd\xff\x00\x00\xee\xff\xec\xff\x00\x00\x00\x00\x00\x00\xe4\xff\xfc\xff\xe3\xff\x00\x00\xe7\xff\xe8\xff\xf1\xff\xe6\xff\xe4\xff\xe2\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x08\x00\x00\x00\x01\x00\x09\x00\x00\x00\x01\x00\x00\x00\x01\x00\x0c\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0b\x00\x0c\x00\x00\x00\x01\x00\x01\x00\x01\x00\x03\x00\x00\x00\x04\x00\x00\x00\x09\x00\x01\x00\x01\x00\x0b\x00\x0b\x00\x0b\x00\x09\x00\x0a\x00\x09\x00\x0a\x00\x00\x00\x0b\x00\x01\x00\x02\x00\x00\x00\x00\x00\x09\x00\x01\x00\x02\x00\x09\x00\x09\x00\x0a\x00\x00\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x09\x00\x07\x00\x08\x00\x04\x00\x05\x00\x07\x00\x08\x00\x00\x00\x02\x00\x06\x00\x09\x00\x04\x00\x05\x00\x04\x00\x06\x00\x03\x00\x03\x00\x07\x00\x04\x00\x02\x00\x09\x00\x05\x00\x03\x00\x03\x00\x00\x00\xff\xff\xff\xff\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x1d\x00\x1e\x00\x05\x00\x1d\x00\x1e\x00\x03\x00\x1d\x00\x1e\x00\x1d\x00\x1e\x00\xff\xff\x1f\x00\x2c\x00\x2d\x00\x1f\x00\x2c\x00\x33\x00\x1f\x00\x20\x00\x1f\x00\x28\x00\x1d\x00\x1e\x00\x2b\x00\x2b\x00\x30\x00\x11\x00\x33\x00\x11\x00\x03\x00\x2b\x00\x0d\x00\x2e\x00\x2c\x00\x2c\x00\x1a\x00\x1b\x00\x1a\x00\x26\x00\x11\x00\x2c\x00\x22\x00\x23\x00\x11\x00\x11\x00\x03\x00\x14\x00\x15\x00\x12\x00\x03\x00\x24\x00\x0d\x00\x18\x00\x30\x00\x03\x00\x0d\x00\x07\x00\x03\x00\x0e\x00\x0f\x00\x08\x00\x09\x00\x0e\x00\x19\x00\x07\x00\x07\x00\x11\x00\x03\x00\x08\x00\x0a\x00\x17\x00\x18\x00\x16\x00\x25\x00\x1d\x00\x26\x00\x29\x00\x03\x00\x28\x00\x32\x00\x03\x00\x05\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 29) [
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
	(29 , happyReduce_29)
	]

happy_n_terms = 13 :: Prelude.Int
happy_n_nonterms = 14 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1), Omni.Abs.Ident (tokenText happy_var_1))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1), (read (Data.Text.unpack (tokenText happy_var_1))) :: Integer)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1), Omni.Abs.InfixOpIdent (tokenText happy_var_1))
	)}

happyReduce_4 = happyReduce 4# 3# happyReduction_4
happyReduction_4 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	happyIn7
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4), Omni.Abs.Module (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_4)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_5 = happyReduce 6# 4# happyReduction_5
happyReduction_5 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut13 happy_x_4 of { (HappyWrap13 happy_var_4) -> 
	case happyOut16 happy_x_6 of { (HappyWrap16 happy_var_6) -> 
	happyIn8
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_6), Omni.Abs.FnDef (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_6)) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn9
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_7 = happySpecReduce_2  5# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	happyIn9
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_2), (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_8 = happySpecReduce_0  6# happyReduction_8
happyReduction_8  =  happyIn10
		 ((Omni.Abs.BNFC'NoPosition, Omni.Abs.NoParams Omni.Abs.BNFC'NoPosition)
	)

happyReduce_9 = happySpecReduce_3  6# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), Omni.Abs.SomeParams (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3))) (snd happy_var_2))
	)}}}

happyReduce_10 = happySpecReduce_3  7# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut13 happy_x_3 of { (HappyWrap13 happy_var_3) -> 
	happyIn11
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Omni.Abs.Param (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_11 = happySpecReduce_0  8# happyReduction_11
happyReduction_11  =  happyIn12
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_12 = happySpecReduce_1  8# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn12
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_13 = happySpecReduce_3  8# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_14 = happyReduce 5# 9# happyReduction_14
happyReduction_14 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	case happyOut13 happy_x_5 of { (HappyWrap13 happy_var_5) -> 
	happyIn13
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_5), Omni.Abs.TFn (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (fst happy_var_5)) (snd happy_var_2) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_15 = happySpecReduce_1  9# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)), Omni.Abs.TUnit (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1))))
	)}

happyReduce_16 = happySpecReduce_1  9# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn13
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.TNamed (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_17 = happySpecReduce_0  10# happyReduction_17
happyReduction_17  =  happyIn14
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_18 = happySpecReduce_1  10# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn14
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_19 = happySpecReduce_3  10# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	case happyOut14 happy_x_3 of { (HappyWrap14 happy_var_3) -> 
	happyIn14
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_20 = happySpecReduce_1  11# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn15
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.EIdent (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_21 = happySpecReduce_1  11# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn15
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), Omni.Abs.EIntLit (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1)) (snd happy_var_1))
	)}

happyReduce_22 = happySpecReduce_1  11# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)), Omni.Abs.EUnit (Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1))))
	)}

happyReduce_23 = happySpecReduce_3  11# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { (HappyWrap16 happy_var_2) -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn15
		 ((Omni.Abs.spanBNFC'Position (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_1)) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_3)), (snd happy_var_2))
	)}}}

happyReduce_24 = happySpecReduce_3  12# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
	case happyOut15 happy_x_3 of { (HappyWrap15 happy_var_3) -> 
	happyIn16
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), Omni.Abs.EInfixOp (Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3)) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_25 = happyReduce 4# 12# happyReduction_25
happyReduction_25 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn16
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_4)), Omni.Abs.EApplication (Omni.Abs.spanBNFC'Position (fst happy_var_1) (uncurry Omni.Abs.BNFC'Position (tokenSpan happy_var_4))) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_26 = happySpecReduce_1  12# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn16
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (snd happy_var_1))
	)}

happyReduce_27 = happySpecReduce_0  13# happyReduction_27
happyReduction_27  =  happyIn17
		 ((Omni.Abs.BNFC'NoPosition, [])
	)

happyReduce_28 = happySpecReduce_1  13# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn17
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_1), (:[]) (snd happy_var_1))
	)}

happyReduce_29 = happySpecReduce_3  13# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn17
		 ((Omni.Abs.spanBNFC'Position (fst happy_var_1) (fst happy_var_3), (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyNewToken action sts stk [] =
	happyDoAction 12# notHappyAtAll action sts stk []

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
	PT _ _ (TV _) -> cont 9#;
	PT _ _ (TI _) -> cont 10#;
	PT _ _ (T_InfixOpIdent _) -> cont 11#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 12# tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap7 x') = happyOut7 x} in x'))

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
