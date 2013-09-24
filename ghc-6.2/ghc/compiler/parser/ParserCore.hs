{-# OPTIONS -fglasgow-exts -cpp #-}
-- parser produced by Happy Version 1.14

module ParserCore ( parseCore ) where

import ForeignCall

import HsCore
import RdrHsSyn
import HsSyn
import TyCon
import TcType
import RdrName
import OccName
import Module
import ParserCoreUtils
import LexCore
import Literal
import BasicTypes
import Type
import SrcLoc
import PrelNames
import FastString
import Outputable

#include "../HsVersions.h"
import Array
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn4 :: (RdrNameHsModule) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (RdrNameHsModule)
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([RdrNameHsDecl]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([RdrNameHsDecl])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (RdrNameHsDecl) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (RdrNameHsDecl)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ((RdrName -> DataConDetails (ConDecl RdrName))) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ((RdrName -> DataConDetails (ConDecl RdrName)))
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (HsTyVarBndr RdrName) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (HsTyVarBndr RdrName)
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([HsTyVarBndr RdrName]) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([HsTyVarBndr RdrName])
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([[RdrNameHsDecl]]) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([[RdrNameHsDecl]])
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([RdrNameHsDecl]) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([RdrNameHsDecl])
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (UfBinding RdrName) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (UfBinding RdrName)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([RdrNameCoreDecl]) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([RdrNameCoreDecl])
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (RdrNameCoreDecl) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (RdrNameCoreDecl)
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ((RdrName, RdrNameHsType)) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ((RdrName, RdrNameHsType))
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([(RdrName, RdrNameHsType)]) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([(RdrName, RdrNameHsType)])
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (UfBinder RdrName) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (UfBinder RdrName)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ([UfBinder RdrName]) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ([UfBinder RdrName])
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([RdrNameHsTyVar]) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([RdrNameHsTyVar])
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Kind) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Kind)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Kind) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Kind)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([ConDecl RdrName]) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([ConDecl RdrName])
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (ConDecl RdrName) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (ConDecl RdrName)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([ RdrNameHsType]) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([ RdrNameHsType])
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (RdrNameHsType) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (RdrNameHsType)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (RdrNameHsType) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (RdrNameHsType)
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (RdrNameHsType) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (RdrNameHsType)
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (UfExpr RdrName) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (UfExpr RdrName)
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (UfExpr RdrName) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (UfExpr RdrName)
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (UfExpr RdrName) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (UfExpr RdrName)
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ([UfAlt RdrName]) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ([UfAlt RdrName])
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (UfAlt RdrName) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (UfAlt RdrName)
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Literal) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Literal)
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (RdrName) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (RdrName)
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (String) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (String)
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (String) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (String)
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (ModuleName) -> (HappyAbsSyn )
happyIn37 x = unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (ModuleName)
happyOut37 x = unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (RdrName) -> (HappyAbsSyn )
happyIn38 x = unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (RdrName)
happyOut38 x = unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (RdrName) -> (HappyAbsSyn )
happyIn39 x = unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (RdrName)
happyOut39 x = unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (RdrName) -> (HappyAbsSyn )
happyIn40 x = unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (RdrName)
happyOut40 x = unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (RdrName) -> (HappyAbsSyn )
happyIn41 x = unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (RdrName)
happyOut41 x = unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x63\x01\x63\x01\x4f\x01\x2b\x01\xc1\x00\x00\x00\x3e\x00\x4e\x01\x46\x01\x46\x01\x45\x01\x2e\x00\x00\x00\x2e\x00\xc1\x00\x00\x00\x4c\x01\x00\x00\x00\x00\x44\x01\x47\x01\x00\x00\x49\x01\x00\x00\xed\x00\x42\x00\xe5\x00\x3e\x00\x00\x00\x2e\x00\x43\x01\x00\x00\x39\x01\x3d\x01\x32\x01\x00\x00\x00\x00\x00\x00\x42\x00\x37\x01\x38\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x34\x01\x00\x00\x00\x00\x2e\x00\x42\x00\x31\x01\x26\x01\xdf\x00\x00\x00\x30\x01\x24\x01\x67\x00\x00\x00\x42\x00\x1d\x01\x79\x01\x00\x00\x2a\x01\x79\x01\x00\x00\x00\x00\x00\x00\x23\x01\x11\x01\x1a\x01\x19\x01\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x25\x00\x84\x00\x71\x00\x06\x01\x05\x01\x4b\x00\xf8\xff\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xff\x15\x01\x13\x01\x2e\x00\x14\x01\x0d\x01\x0a\x01\x09\x01\x03\x01\x71\x00\x67\x00\x67\x00\x0f\x01\x0e\x01\x00\x00\xf5\x00\x00\x00\x71\x00\x71\x00\x2e\x00\xf6\x00\xf3\x00\x00\x00\xf9\x00\xe8\x00\x00\x00\x00\x00\x79\x01\x00\x00\x00\x00\xef\x00\x00\x00\x71\x00\x00\x00\xdf\x00\x67\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x71\x00\x71\x00\x71\x00\x71\x00\x00\x00\x00\x00\xc5\x00\x67\x00\x00\x00\x00\x00\x42\x00\xe4\x00\xb9\x00\xb7\x00\xb0\x00\xa5\x00\x00\x00\xa2\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x00\x00\x00\x8c\x00\x87\x00\x83\x00\x6a\x00\x81\x00\x45\x00\x67\x00\x6d\x00\x67\x00\xf6\xff\x00\x00\x00\x00\x00\x00\x6d\x00\x60\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x70\x00\x00\x00\x53\x00\x00\x00\xcb\x00\x00\x00\x01\x00\x00\x00\x3d\x00\x1d\x00\x00\x00\x3a\x00\x00\x00\x36\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x00\xba\x00\x4f\x00\xfb\xff\x00\x00\x30\x00\x00\x00\x00\x00\x43\x00\x59\x00\x29\x00\x00\x00\x00\x00\x00\x00\xab\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\x00\x00\x00\x00\x00\x00\x00\x10\x00\xa7\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x56\x01\x00\x00\x98\x00\x65\x00\x34\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x68\x01\x00\x00\x00\x00\x00\x00\x07\x00\x5f\x01\xd6\x00\x00\x00\x00\x00\x48\x01\xc7\x00\x94\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\x00\x00\x00\x04\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x3a\x01\x2c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xce\x00\x6c\x00\x06\x00\xfd\xff\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x09\x00\x00\x00\x68\x00\x00\x00\x0d\x00\x1e\x01\xfe\xff\x00\x00\x00\x00\x00\x00\xca\x00\xc6\x00\xc2\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x85\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x02\x01\x88\x00\xf4\x00\x74\x01\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\xe6\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xb5\xff\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xb6\xff\xf5\xff\xfd\xff\xfe\xff\x00\x00\xf0\xff\xb4\xff\x00\x00\x00\x00\xb2\xff\x00\x00\xb8\xff\x00\x00\x00\x00\x00\x00\xf3\xff\xfc\xff\xf5\xff\x00\x00\xf7\xff\x00\x00\xf9\xff\x00\x00\xb1\xff\xb7\xff\xfa\xff\x00\x00\x00\x00\x00\x00\xf4\xff\xf2\xff\xaf\xff\xb3\xff\xd3\xff\xd1\xff\x00\x00\xd6\xff\xd5\xff\xf5\xff\x00\x00\x00\x00\xed\xff\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\x00\x00\x00\x00\xf8\xff\x00\x00\x00\x00\xe0\xff\xe1\xff\xdf\xff\x00\x00\xdb\xff\x00\x00\xe3\xff\xd0\xff\xc9\xff\xc8\xff\xeb\xff\xcd\xff\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\xff\xec\xff\xcf\xff\xe6\xff\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\x00\x00\xcb\xff\x00\x00\xd8\xff\x00\x00\x00\x00\x00\x00\xfb\xff\xdd\xff\x00\x00\xf6\xff\xde\xff\x00\x00\xda\xff\xb0\xff\xe3\xff\xd9\xff\xd8\xff\xca\xff\x00\x00\x00\x00\x00\x00\xc4\xff\xc3\xff\xc2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xe7\xff\x00\x00\x00\x00\xe4\xff\xc7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\x00\x00\xd7\xff\xe2\xff\xdc\xff\xef\xff\x00\x00\xba\xff\xb9\xff\xbb\xff\xbc\xff\x00\x00\xea\xff\x00\x00\xc1\xff\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\xe9\xff\x00\x00\x00\x00\xc5\xff\xc0\xff\xbe\xff\xe9\xff\x00\x00\xbd\xff\x00\x00\xe8\xff\xbf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x06\x00\x07\x00\x0d\x00\x0e\x00\x0a\x00\x0e\x00\x06\x00\x07\x00\x0b\x00\x04\x00\x0a\x00\x12\x00\x13\x00\x04\x00\x08\x00\x18\x00\x0a\x00\x0f\x00\x1d\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x0f\x00\x1e\x00\x20\x00\x20\x00\x1f\x00\x22\x00\x24\x00\x1e\x00\x25\x00\x20\x00\x1e\x00\x22\x00\x1e\x00\x1e\x00\x25\x00\x20\x00\x1e\x00\x22\x00\x05\x00\x1e\x00\x25\x00\x20\x00\x1e\x00\x22\x00\x09\x00\x0a\x00\x25\x00\x0f\x00\x04\x00\x05\x00\x09\x00\x0a\x00\x10\x00\x11\x00\x04\x00\x05\x00\x0e\x00\x20\x00\x04\x00\x05\x00\x23\x00\x1c\x00\x1d\x00\x05\x00\x10\x00\x1e\x00\x04\x00\x20\x00\x1f\x00\x22\x00\x1c\x00\x1e\x00\x25\x00\x20\x00\x1e\x00\x22\x00\x0e\x00\x06\x00\x25\x00\x08\x00\x1e\x00\x0a\x00\x0b\x00\x0c\x00\x1e\x00\x0e\x00\x1c\x00\x1d\x00\x03\x00\x20\x00\x1c\x00\x1d\x00\x23\x00\x1e\x00\x17\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x06\x00\x1f\x00\x08\x00\x00\x00\x0a\x00\x0b\x00\x0c\x00\x21\x00\x0e\x00\x16\x00\x12\x00\x13\x00\x0b\x00\x0c\x00\x0e\x00\x14\x00\x15\x00\x17\x00\x0e\x00\x14\x00\x15\x00\x18\x00\x1c\x00\x1d\x00\x20\x00\x1e\x00\x0e\x00\x20\x00\x24\x00\x1e\x00\x23\x00\x20\x00\x1c\x00\x1d\x00\x23\x00\x0e\x00\x18\x00\x0e\x00\x0b\x00\x0c\x00\x1c\x00\x1d\x00\x16\x00\x16\x00\x16\x00\x15\x00\x16\x00\x17\x00\x11\x00\x1c\x00\x1d\x00\x1c\x00\x1d\x00\x1b\x00\x1e\x00\x0b\x00\x20\x00\x0d\x00\x0e\x00\x23\x00\x15\x00\x16\x00\x17\x00\x0f\x00\x15\x00\x16\x00\x17\x00\x10\x00\x11\x00\x1e\x00\x11\x00\x20\x00\x10\x00\x1e\x00\x23\x00\x20\x00\x01\x00\x02\x00\x23\x00\x15\x00\x16\x00\x17\x00\x0f\x00\x15\x00\x16\x00\x17\x00\x02\x00\x03\x00\x1e\x00\x0f\x00\x20\x00\x0f\x00\x1e\x00\x23\x00\x20\x00\x01\x00\x02\x00\x23\x00\x15\x00\x16\x00\x17\x00\x0b\x00\x15\x00\x0d\x00\x0e\x00\x0e\x00\x15\x00\x1e\x00\x14\x00\x20\x00\x15\x00\x1e\x00\x23\x00\x20\x00\x15\x00\x1e\x00\x23\x00\x20\x00\x15\x00\x1e\x00\x23\x00\x20\x00\x15\x00\x1e\x00\x23\x00\x20\x00\x15\x00\x1e\x00\x23\x00\x20\x00\x15\x00\x1e\x00\x23\x00\x20\x00\x0f\x00\x1e\x00\x23\x00\x20\x00\x0f\x00\x1e\x00\x23\x00\x20\x00\x1c\x00\x1d\x00\x23\x00\x18\x00\x19\x00\x1a\x00\x1c\x00\x1d\x00\x1d\x00\x1e\x00\x10\x00\x20\x00\x18\x00\x22\x00\x1c\x00\x1d\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x16\x00\x1d\x00\x1d\x00\x1e\x00\x1d\x00\x20\x00\x07\x00\x22\x00\x14\x00\x09\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x14\x00\x14\x00\x1d\x00\x1e\x00\x14\x00\x20\x00\x0f\x00\x22\x00\x20\x00\x20\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x16\x00\x1b\x00\x1d\x00\x1e\x00\x1c\x00\x20\x00\x18\x00\x22\x00\x19\x00\x11\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x0f\x00\x1d\x00\x1d\x00\x1e\x00\x19\x00\x20\x00\x0f\x00\x22\x00\x1b\x00\x11\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x13\x00\x10\x00\x1d\x00\x1e\x00\x14\x00\x20\x00\x22\x00\x22\x00\x1d\x00\x13\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x1c\x00\x13\x00\x1d\x00\x1e\x00\x10\x00\x20\x00\x14\x00\x22\x00\x19\x00\x19\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x1d\x00\x01\x00\x1d\x00\x1e\x00\x1b\x00\x20\x00\x1b\x00\x22\x00\xff\xff\x1d\x00\x25\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x1d\x00\x1e\x00\xff\xff\x20\x00\x18\x00\x22\x00\xff\xff\xff\xff\x25\x00\x1d\x00\x1e\x00\xff\xff\x20\x00\x18\x00\x22\x00\xff\xff\xff\xff\x25\x00\x1d\x00\x1e\x00\x0e\x00\x20\x00\xff\xff\x22\x00\x12\x00\xff\xff\x25\x00\x15\x00\x1b\x00\x1c\x00\x1d\x00\xff\xff\x1a\x00\x20\x00\x1b\x00\x1c\x00\x1d\x00\x24\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\x24\x00\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x2a\x00\x10\x00\xa5\x00\xa6\x00\x11\x00\x5e\x00\x0f\x00\x10\x00\x92\x00\x79\x00\x11\x00\x77\x00\x46\x00\x88\x00\x68\x00\x5f\x00\x69\x00\xa7\x00\x0d\x00\x1d\x00\x39\x00\x94\x00\x35\x00\x96\x00\x12\x00\x47\x00\x13\x00\x78\x00\x14\x00\x48\x00\x12\x00\x15\x00\x13\x00\x89\x00\x14\x00\x1f\x00\x12\x00\x15\x00\x13\x00\x1f\x00\x14\x00\x6b\x00\x12\x00\x15\x00\x13\x00\x1f\x00\x14\x00\x58\x00\x35\x00\x15\x00\x6d\x00\x1d\x00\x29\x00\x34\x00\x35\x00\x72\x00\x97\x00\x1d\x00\x1e\x00\x21\x00\x0a\x00\x1d\x00\x21\x00\x0b\x00\x18\x00\x0d\x00\x17\x00\x40\x00\x12\x00\x33\x00\x13\x00\x23\x00\x14\x00\x18\x00\x12\x00\x15\x00\x13\x00\x1f\x00\x14\x00\x34\x00\x50\x00\x15\x00\x51\x00\x1f\x00\x52\x00\x53\x00\x54\x00\x1f\x00\x55\x00\x18\x00\x0d\x00\x25\x00\x0a\x00\x18\x00\x0d\x00\x0d\x00\x27\x00\x56\x00\x61\x00\x62\x00\x63\x00\x64\x00\x18\x00\x0d\x00\x61\x00\x62\x00\x63\x00\x64\x00\x50\x00\x2b\x00\x51\x00\x03\x00\x52\x00\x53\x00\x54\x00\x04\x00\x55\x00\xb1\x00\x45\x00\x46\x00\xad\x00\xb1\x00\x5e\x00\x95\x00\x7b\x00\x56\x00\x34\x00\x7a\x00\x7b\x00\x6f\x00\x18\x00\x0d\x00\x47\x00\x30\x00\x55\x00\x0a\x00\x48\x00\x30\x00\x31\x00\x0a\x00\x18\x00\x0d\x00\x31\x00\x34\x00\x6d\x00\x55\x00\xad\x00\xae\x00\x18\x00\x0d\x00\xa7\x00\x3d\x00\xa9\x00\x2d\x00\x2e\x00\x9e\x00\xab\x00\x18\x00\x0d\x00\x18\x00\x0d\x00\xaa\x00\x30\x00\x5a\x00\x0a\x00\x5b\x00\x8b\x00\x31\x00\x2d\x00\x2e\x00\x59\x00\xa0\x00\x2d\x00\x2e\x00\x49\x00\x72\x00\x73\x00\x30\x00\x99\x00\x0a\x00\x9a\x00\x30\x00\x31\x00\x0a\x00\x1c\x00\x07\x00\x31\x00\x2d\x00\x2e\x00\x38\x00\x9b\x00\x2d\x00\x2e\x00\x3f\x00\x09\x00\x0a\x00\x30\x00\x9c\x00\x0a\x00\x9d\x00\x30\x00\x31\x00\x0a\x00\x06\x00\x07\x00\x31\x00\x2d\x00\x2e\x00\x2f\x00\x5a\x00\x8e\x00\x5b\x00\x5c\x00\x5e\x00\x8f\x00\x30\x00\x8e\x00\x0a\x00\x90\x00\x30\x00\x31\x00\x0a\x00\x91\x00\x30\x00\x31\x00\x0a\x00\x7c\x00\x30\x00\x31\x00\x0a\x00\x82\x00\x30\x00\x31\x00\x0a\x00\x66\x00\x30\x00\x31\x00\x0a\x00\x3b\x00\x30\x00\x31\x00\x0a\x00\x9e\x00\x30\x00\x31\x00\x0a\x00\x76\x00\x30\x00\x31\x00\x0a\x00\x18\x00\x0d\x00\x31\x00\x4a\x00\x4b\x00\xb2\x00\x2d\x00\x25\x00\x4d\x00\x12\x00\x7e\x00\x13\x00\x6f\x00\x4e\x00\x18\x00\x0d\x00\x15\x00\x4a\x00\x4b\x00\xac\x00\x77\x00\x0d\x00\x4d\x00\x12\x00\x25\x00\x13\x00\x7f\x00\x4e\x00\x84\x00\x80\x00\x15\x00\x4a\x00\x4b\x00\xaf\x00\x85\x00\x86\x00\x4d\x00\x12\x00\x87\x00\x13\x00\x88\x00\x4e\x00\x65\x00\x66\x00\x15\x00\x4a\x00\x4b\x00\x8c\x00\x8b\x00\x71\x00\x4d\x00\x12\x00\x18\x00\x13\x00\x6f\x00\x4e\x00\x70\x00\x72\x00\x15\x00\x4a\x00\x4b\x00\x93\x00\x75\x00\x0d\x00\x4d\x00\x12\x00\x57\x00\x13\x00\x58\x00\x4e\x00\x37\x00\x38\x00\x15\x00\x4a\x00\x4b\x00\x80\x00\x3b\x00\x3e\x00\x4d\x00\x12\x00\x3f\x00\x13\x00\xff\xff\x4e\x00\x25\x00\x27\x00\x15\x00\x4a\x00\x4b\x00\x81\x00\x18\x00\x29\x00\x4d\x00\x12\x00\x19\x00\x13\x00\x1a\x00\x4e\x00\x1b\x00\x23\x00\x15\x00\x4a\x00\x4b\x00\x5f\x00\x0d\x00\x03\x00\x4d\x00\x12\x00\x1c\x00\x13\x00\x0f\x00\x4e\x00\x00\x00\x06\x00\x15\x00\x4a\x00\x4b\x00\x4c\x00\x00\x00\x00\x00\x4d\x00\x12\x00\x00\x00\x13\x00\x67\x00\x4e\x00\x00\x00\x00\x00\x15\x00\x4d\x00\x12\x00\x00\x00\x13\x00\x6b\x00\x4e\x00\x00\x00\x00\x00\x15\x00\x4d\x00\x12\x00\x42\x00\x13\x00\x00\x00\x4e\x00\x43\x00\x00\x00\x15\x00\x44\x00\xab\x00\xa1\x00\xa2\x00\x00\x00\x45\x00\x47\x00\xa0\x00\xa1\x00\xa2\x00\xa3\x00\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\xa3\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 80) [
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
	(80 , happyReduce_80)
	]

happy_n_terms = 35 :: Int
happy_n_nonterms = 38 :: Int

happyReduce_1 = happyReduce 4# 0# happyReduction_1
happyReduction_1 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	happyIn4
		 (HsModule (Just (mkHomeModule happy_var_2)) Nothing 
		           [] (happy_var_3 ++ concat happy_var_4) Nothing noSrcLoc
	) `HappyStk` happyRest}}}

happyReduce_2 = happySpecReduce_0 1# happyReduction_2
happyReduction_2  =  happyIn5
		 ([]
	)

happyReduce_3 = happySpecReduce_3 1# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (happy_var_1:happy_var_3
	)}}

happyReduce_4 = happyReduce 7# 2# happyReduction_4
happyReduction_4 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut22 happy_x_6 of { happy_var_6 -> 
	happyIn6
		 (TyClD (mkTyData DataType ([], happy_var_2, happy_var_3) (DataCons happy_var_6) Nothing noSrcLoc)
	) `HappyStk` happyRest}}}

happyReduce_5 = happyReduce 4# 2# happyReduction_5
happyReduction_5 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn6
		 (TyClD (mkTyData NewType ([], happy_var_2, happy_var_3) (happy_var_4 happy_var_2) Nothing noSrcLoc)
	) `HappyStk` happyRest}}}

happyReduce_6 = happySpecReduce_0 3# happyReduction_6
happyReduction_6  =  happyIn7
		 ((\ tc_name -> Unknown)
	)

happyReduce_7 = happySpecReduce_2 3# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 ((\ tc_name -> let { dc_name  = setRdrNameSpace tc_name dataName ;
			                      con_info = PrefixCon [unbangedType happy_var_2] }
			                in DataCons [ConDecl dc_name [] [] con_info noSrcLoc])
	)}

happyReduce_8 = happySpecReduce_1 4# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (IfaceTyVar happy_var_1 liftedTypeKind
	)}

happyReduce_9 = happyReduce 5# 4# happyReduction_9
happyReduction_9 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	happyIn8
		 (IfaceTyVar happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_10 = happySpecReduce_0 5# happyReduction_10
happyReduction_10  =  happyIn9
		 ([]
	)

happyReduce_11 = happySpecReduce_2 5# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (happy_var_1:happy_var_2
	)}}

happyReduce_12 = happySpecReduce_0 6# happyReduction_12
happyReduction_12  =  happyIn10
		 ([]
	)

happyReduce_13 = happySpecReduce_3 6# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((happy_var_1:happy_var_3)
	)}}

happyReduce_14 = happyReduce 4# 7# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (map CoreD happy_var_3
	) `HappyStk` happyRest}

happyReduce_15 = happySpecReduce_1 7# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ([CoreD happy_var_1]
	)}

happyReduce_16 = happyReduce 4# 8# happyReduction_16
happyReduction_16 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (UfRec (map convBind happy_var_3)
	) `HappyStk` happyRest}

happyReduce_17 = happySpecReduce_1 8# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (let (b,r) = convBind happy_var_1
				  in UfNonRec b r
	)}

happyReduce_18 = happySpecReduce_1 9# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ([happy_var_1]
	)}

happyReduce_19 = happySpecReduce_3 9# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (happy_var_1:happy_var_3
	)}}

happyReduce_20 = happyReduce 5# 10# happyReduction_20
happyReduction_20 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_5 of { happy_var_5 -> 
	happyIn14
		 (CoreDecl happy_var_1 happy_var_3 happy_var_5 noSrcLoc
	) `HappyStk` happyRest}}}

happyReduce_21 = happyReduce 5# 11# happyReduction_21
happyReduction_21 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_2 of { happy_var_2 -> 
	case happyOut27 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_22 = happySpecReduce_0 12# happyReduction_22
happyReduction_22  =  happyIn16
		 ([]
	)

happyReduce_23 = happySpecReduce_2 12# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 (happy_var_1:happy_var_2
	)}}

happyReduce_24 = happySpecReduce_2 13# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (let (IfaceTyVar v k) = happy_var_2  in UfTyBinder  v k
	)}

happyReduce_25 = happySpecReduce_1 13# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (let (v,ty) = happy_var_1 in UfValBinder v ty
	)}

happyReduce_26 = happySpecReduce_1 14# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ([happy_var_1]
	)}

happyReduce_27 = happySpecReduce_2 14# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (happy_var_1:happy_var_2
	)}}

happyReduce_28 = happySpecReduce_0 15# happyReduction_28
happyReduction_28  =  happyIn19
		 ([]
	)

happyReduce_29 = happySpecReduce_3 15# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 (happy_var_2:happy_var_3
	)}}

happyReduce_30 = happySpecReduce_1 16# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn20
		 (liftedTypeKind
	)

happyReduce_31 = happySpecReduce_1 16# happyReduction_31
happyReduction_31 happy_x_1
	 =  happyIn20
		 (unliftedTypeKind
	)

happyReduce_32 = happySpecReduce_1 16# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn20
		 (openTypeKind
	)

happyReduce_33 = happySpecReduce_3 16# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (happy_var_2
	)}

happyReduce_34 = happySpecReduce_1 17# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_35 = happySpecReduce_3 17# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (mkArrowKind happy_var_1 happy_var_3
	)}}

happyReduce_36 = happySpecReduce_1 18# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ([happy_var_1]
	)}

happyReduce_37 = happySpecReduce_3 18# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (happy_var_1:happy_var_3
	)}}

happyReduce_38 = happySpecReduce_3 19# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (ConDecl happy_var_1 happy_var_2 [] (PrefixCon (map unbangedType happy_var_3)) noSrcLoc
	)}}}

happyReduce_39 = happySpecReduce_0 20# happyReduction_39
happyReduction_39  =  happyIn24
		 ([]
	)

happyReduce_40 = happySpecReduce_2 20# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (happy_var_1:happy_var_2
	)}}

happyReduce_41 = happySpecReduce_1 21# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (HsTyVar happy_var_1
	)}

happyReduce_42 = happySpecReduce_1 21# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (HsTyVar happy_var_1
	)}

happyReduce_43 = happySpecReduce_3 21# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (happy_var_2
	)}

happyReduce_44 = happySpecReduce_1 22# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_45 = happySpecReduce_2 22# happyReduction_45
happyReduction_45 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn26
		 (HsAppTy happy_var_1 happy_var_2
	)}}

happyReduce_46 = happySpecReduce_1 23# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_47 = happySpecReduce_3 23# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (HsFunTy happy_var_1 happy_var_3
	)}}

happyReduce_48 = happyReduce 4# 23# happyReduction_48
happyReduction_48 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { happy_var_2 -> 
	case happyOut27 happy_x_4 of { happy_var_4 -> 
	happyIn27
		 (HsForAllTy (Just happy_var_2) [] happy_var_4
	) `HappyStk` happyRest}}

happyReduce_49 = happySpecReduce_1 24# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (UfVar happy_var_1
	)}

happyReduce_50 = happySpecReduce_1 24# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (UfLit happy_var_1
	)}

happyReduce_51 = happySpecReduce_3 24# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (happy_var_2
	)}

happyReduce_52 = happySpecReduce_2 25# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (UfApp happy_var_1 happy_var_2
	)}}

happyReduce_53 = happySpecReduce_3 25# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 (UfApp happy_var_1 (UfType happy_var_3)
	)}}

happyReduce_54 = happySpecReduce_1 25# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_55 = happySpecReduce_1 26# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_56 = happyReduce 4# 26# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn30
		 (foldr UfLam happy_var_4 happy_var_2
	) `HappyStk` happyRest}}

happyReduce_57 = happyReduce 4# 26# happyReduction_57
happyReduction_57 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn30
		 (UfLet happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_58 = happyReduce 7# 26# happyReduction_58
happyReduction_58 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOut15 happy_x_4 of { happy_var_4 -> 
	case happyOut31 happy_x_6 of { happy_var_6 -> 
	happyIn30
		 (UfCase happy_var_2 (fst happy_var_4) happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_59 = happySpecReduce_3 26# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (UfNote (UfCoerce happy_var_2) happy_var_3
	)}}

happyReduce_60 = happySpecReduce_3 26# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TKstring happy_var_2) -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (case happy_var_2 of
	       --"SCC"        -> UfNote (UfSCC "scc") happy_var_3
	       "InlineCall" -> UfNote UfInlineCall happy_var_3
	       "InlineMe"   -> UfNote UfInlineMe happy_var_3
	)}}

happyReduce_61 = happySpecReduce_3 26# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TKstring happy_var_2) -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (UfFCall (ForeignCall.CCall 
                                               (CCallSpec (StaticTarget 
                                                            (mkFastString happy_var_2)) 
                                                          CCallConv (PlaySafe False))) happy_var_3
	)}}

happyReduce_62 = happySpecReduce_1 27# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ([happy_var_1]
	)}

happyReduce_63 = happySpecReduce_3 27# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (happy_var_1:happy_var_3
	)}}

happyReduce_64 = happyReduce 5# 28# happyReduction_64
happyReduction_64 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 ((UfDataAlt happy_var_1, (map hsTyVarName happy_var_2 ++ map fst happy_var_3), happy_var_5)
	) `HappyStk` happyRest}}}}

happyReduce_65 = happySpecReduce_3 28# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 ((UfLitAlt happy_var_1, [], happy_var_3)
	)}}

happyReduce_66 = happySpecReduce_3 28# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 ((UfDefault, [], happy_var_3)
	)}

happyReduce_67 = happyReduce 5# 29# happyReduction_67
happyReduction_67 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKinteger happy_var_2) -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (convIntLit happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_68 = happyReduce 5# 29# happyReduction_68
happyReduction_68 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKrational happy_var_2) -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (convRatLit happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_69 = happyReduce 5# 29# happyReduction_69
happyReduction_69 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKchar happy_var_2) -> 
	happyIn33
		 (MachChar (fromEnum happy_var_2)
	) `HappyStk` happyRest}

happyReduce_70 = happyReduce 5# 29# happyReduction_70
happyReduction_70 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TKstring happy_var_2) -> 
	happyIn33
		 (MachStr (mkFastString happy_var_2)
	) `HappyStk` happyRest}

happyReduce_71 = happySpecReduce_1 30# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKname happy_var_1) -> 
	happyIn34
		 (mkRdrUnqual (mkVarOccEncoded (mkFastString happy_var_1))
	)}

happyReduce_72 = happySpecReduce_1 31# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKcname happy_var_1) -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_73 = happySpecReduce_1 32# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKcname happy_var_1) -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_74 = happySpecReduce_1 33# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TKcname happy_var_1) -> 
	happyIn37
		 (mkSysModuleNameFS (mkFastString happy_var_1)
	)}

happyReduce_75 = happySpecReduce_1 34# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_76 = happySpecReduce_3 34# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (TKname happy_var_3) -> 
	happyIn38
		 (mkIfaceOrig varName (mkFastString happy_var_1) (mkFastString happy_var_3)
	)}}

happyReduce_77 = happySpecReduce_1 34# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_78 = happySpecReduce_3 35# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (mkIfaceOrig tcName (mkFastString happy_var_1) (mkFastString happy_var_3)
	)}}

happyReduce_79 = happySpecReduce_3 36# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn40
		 (mkIfaceOrig dataName (mkFastString happy_var_1) (mkFastString happy_var_3)
	)}}

happyReduce_80 = happySpecReduce_3 37# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (mkIfaceOrig varName (mkFastString happy_var_1) (mkFastString happy_var_3)
	)}}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TKEOF -> happyDoAction 34# (error "reading EOF!") action sts stk;
	TKmodule -> cont 1#;
	TKdata -> cont 2#;
	TKnewtype -> cont 3#;
	TKforall -> cont 4#;
	TKrec -> cont 5#;
	TKlet -> cont 6#;
	TKin -> cont 7#;
	TKcase -> cont 8#;
	TKof -> cont 9#;
	TKcoerce -> cont 10#;
	TKnote -> cont 11#;
	TKexternal -> cont 12#;
	TKwild -> cont 13#;
	TKoparen -> cont 14#;
	TKcparen -> cont 15#;
	TKobrace -> cont 16#;
	TKcbrace -> cont 17#;
	TKhash -> cont 18#;
	TKeq -> cont 19#;
	TKcoloncolon -> cont 20#;
	TKstar -> cont 21#;
	TKrarrow -> cont 22#;
	TKlambda -> cont 23#;
	TKat -> cont 24#;
	TKdot -> cont 25#;
	TKquestion -> cont 26#;
	TKsemicolon -> cont 27#;
	TKname happy_dollar_dollar -> cont 28#;
	TKcname happy_dollar_dollar -> cont 29#;
	TKinteger happy_dollar_dollar -> cont 30#;
	TKrational happy_dollar_dollar -> cont 31#;
	TKstring happy_dollar_dollar -> cont 32#;
	TKchar happy_dollar_dollar -> cont 33#;
	_ -> happyError
	})

happyThen :: P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: a -> P a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 = happyReturn

parseCore = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq

convBind :: RdrNameCoreDecl -> (UfBinder RdrName, UfExpr RdrName)
convBind (CoreDecl n ty rhs _) = (UfValBinder n ty, rhs)

convIntLit :: Integer -> RdrNameHsType -> Literal
convIntLit i (HsTyVar n)
  | n == intPrimRdrName  = MachInt  i  
  | n == wordPrimRdrName = MachWord i
  | n == charPrimRdrName = MachChar (fromInteger i)
  | n == addrPrimRdrName && i == 0 = MachNullAddr
convIntLit i aty
  = pprPanic "Unknown integer literal type" (ppr aty $$ ppr intPrimRdrName) 

convRatLit :: Rational -> RdrNameHsType -> Literal
convRatLit r (HsTyVar n)
  | n == floatPrimRdrName  = MachFloat  r
  | n == doublePrimRdrName = MachDouble r
convRatLit i aty
  = pprPanic "Unknown rational literal type" (ppr aty $$ ppr intPrimRdrName) 


wordPrimRdrName, intPrimRdrName, floatPrimRdrName, doublePrimRdrName, addrPrimRdrName :: RdrName
wordPrimRdrName   = nameRdrName wordPrimTyConName
intPrimRdrName    = nameRdrName intPrimTyConName
charPrimRdrName   = nameRdrName charPrimTyConName
floatPrimRdrName  = nameRdrName floatPrimTyConName
doublePrimRdrName = nameRdrName doublePrimTyConName
addrPrimRdrName   = nameRdrName addrPrimTyConName

happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id$













{-# LINE 27 "GenericTemplate.hs" #-}



data Happy_IntList = HappyCons Int# Happy_IntList






































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = (happyTcHack j 
				                  (happyTcHack st))
					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st











indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 165 "GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
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
