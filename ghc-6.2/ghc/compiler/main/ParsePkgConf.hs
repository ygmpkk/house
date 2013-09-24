{-# OPTIONS -fglasgow-exts -cpp #-}
-- parser produced by Happy Version 1.14

module ParsePkgConf( loadPackageConfig ) where

#include "HsVersions.h"

import Packages  ( PackageConfig(..), defaultPackageConfig )
import Lexer
import CmdLineOpts
import FastString
import StringBuffer
import SrcLoc
import Outputable
import Panic     ( GhcException(..) )
import EXCEPTION ( throwDyn )
import Array
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn4 :: ([ PackageConfig ]) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ([ PackageConfig ])
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([ PackageConfig ]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([ PackageConfig ])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (PackageConfig) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (PackageConfig)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (PackageConfig -> PackageConfig) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (PackageConfig -> PackageConfig)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (PackageConfig -> PackageConfig) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (PackageConfig -> PackageConfig)
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([String]) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([String])
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([String]) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([String])
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Bool) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Bool)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x1f\x00\x1e\x00\x1c\x00\x13\x00\x01\x00\x0e\x00\x00\x00\x00\x00\x1b\x00\x18\x00\x00\x00\x16\x00\x00\x00\x08\x00\x00\x00\x15\x00\xfe\xff\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x17\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xfe\xff\x00\x00\x00\x00\xfd\xff\x00\x00\xfb\xff\x00\x00\xf9\xff\x00\x00\x00\x00\xfa\xff\x00\x00\xf8\xff\xf5\xff\xf6\xff\x00\x00\xf0\xff\xf7\xff\x00\x00\xf4\xff\xf2\xff\xf3\xff\x00\x00\xf1\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x05\x00\x04\x00\x07\x00\x04\x00\x08\x00\x09\x00\x09\x00\x08\x00\x02\x00\x01\x00\x02\x00\x05\x00\x03\x00\x04\x00\x04\x00\x05\x00\x04\x00\x05\x00\x04\x00\x06\x00\x02\x00\x00\x00\xff\xff\x07\x00\x09\x00\x06\x00\x01\x00\x0a\x00\x08\x00\x07\x00\x04\x00\x03\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x17\x00\x14\x00\x1b\x00\x15\x00\x08\x00\x18\x00\x19\x00\x1c\x00\x09\x00\x12\x00\x05\x00\x06\x00\x13\x00\x0d\x00\x0e\x00\x1d\x00\x1e\x00\x0b\x00\x0c\x00\x13\x00\x19\x00\x0c\x00\x03\x00\x00\x00\x10\x00\x1f\x00\x11\x00\x0a\x00\xff\xff\x09\x00\x10\x00\x08\x00\x03\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 15) [
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
	(15 , happyReduce_15)
	]

happy_n_terms = 11 :: Int
happy_n_nonterms = 8 :: Int

happyReduce_1 = happySpecReduce_2 0# happyReduction_1
happyReduction_1 happy_x_2
	happy_x_1
	 =  happyIn4
		 ([]
	)

happyReduce_2 = happySpecReduce_3 0# happyReduction_2
happyReduction_2 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (reverse happy_var_2
	)}

happyReduce_3 = happySpecReduce_1 1# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([ happy_var_1 ]
	)}

happyReduce_4 = happySpecReduce_3 1# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_5 = happyReduce 4# 2# happyReduction_5
happyReduction_5 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (happy_var_3 defaultPackageConfig
	) `HappyStk` happyRest}

happyReduce_6 = happySpecReduce_1 3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (\p -> happy_var_1 p
	)}

happyReduce_7 = happySpecReduce_3 3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (\p -> happy_var_1 (happy_var_3 p)
	)}}

happyReduce_8 = happyMonadReduce 3# 4# happyReduction_8
happyReduction_8 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOutTok happy_x_1 of { (T _ _ (ITvarid    happy_var_1)) -> 
	case happyOutTok happy_x_3 of { (T _ _ (ITstring   happy_var_3)) -> 
	 case unpackFS happy_var_1 of { 
		   "name" -> return (\ p -> p{name = unpackFS happy_var_3});
		   _      -> happyError }}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_9 = happySpecReduce_3 4# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T _ _ (ITvarid    happy_var_1)) -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (\p -> case unpackFS happy_var_1 of {
		   	"auto" -> p{auto = happy_var_3};
		   	_      -> p }
	)}}

happyReduce_10 = happySpecReduce_3 4# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (T _ _ (ITvarid    happy_var_1)) -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (\p -> case unpackFS happy_var_1 of
		        "import_dirs"     -> p{import_dirs     = happy_var_3}
		        "library_dirs"    -> p{library_dirs    = happy_var_3}
		        "hs_libraries"    -> p{hs_libraries    = happy_var_3}
		        "extra_libraries" -> p{extra_libraries = happy_var_3}
		        "include_dirs"    -> p{include_dirs    = happy_var_3}
		        "c_includes"      -> p{c_includes      = happy_var_3}
		        "package_deps"    -> p{package_deps    = happy_var_3}
		        "extra_ghc_opts"  -> p{extra_ghc_opts  = happy_var_3}
		        "extra_cc_opts"   -> p{extra_cc_opts   = happy_var_3}
		        "extra_ld_opts"   -> p{extra_ld_opts   = happy_var_3}
		        "framework_dirs"  -> p{framework_dirs  = happy_var_3}
		        "extra_frameworks"-> p{extra_frameworks= happy_var_3}
			_other            -> p
	)}}

happyReduce_11 = happySpecReduce_2 5# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  happyIn9
		 ([]
	)

happyReduce_12 = happySpecReduce_3 5# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (reverse happy_var_2
	)}

happyReduce_13 = happySpecReduce_1 6# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { (T _ _ (ITstring   happy_var_1)) -> 
	happyIn10
		 ([ unpackFS happy_var_1 ]
	)}

happyReduce_14 = happySpecReduce_3 6# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (T _ _ (ITstring   happy_var_3)) -> 
	happyIn10
		 (unpackFS happy_var_3 : happy_var_1
	)}}

happyReduce_15 = happyMonadReduce 1# 7# happyReduction_15
happyReduction_15 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOutTok happy_x_1 of { (T _ _ (ITconid    happy_var_1)) -> 
	 case unpackFS happy_var_1 of {
					    "True"  -> return True;
					    "False" -> return False;
					    _       -> happyError }}
	) (\r -> happyReturn (happyIn11 r))

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	T _ _ ITeof -> happyDoAction 10# (error "reading EOF!") action sts stk;
	T _ _ ITocurly -> cont 1#;
	T _ _ ITccurly -> cont 2#;
	T _ _ ITobrack -> cont 3#;
	T _ _ ITcbrack -> cont 4#;
	T _ _ ITcomma -> cont 5#;
	T _ _ ITequal -> cont 6#;
	T _ _ (ITvarid    happy_dollar_dollar) -> cont 7#;
	T _ _ (ITconid    happy_dollar_dollar) -> cont 8#;
	T _ _ (ITstring   happy_dollar_dollar) -> cont 9#;
	_ -> happyError
	})

happyThen :: P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 = happyReturn

parse = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq

happyError :: P a
happyError = srcParseFail

loadPackageConfig :: FilePath -> IO [PackageConfig]
loadPackageConfig conf_filename = do
   buf <- hGetStringBuffer conf_filename
   let loc  = mkSrcLoc (mkFastString conf_filename) 1 0
   case unP parse (mkPState buf loc defaultDynFlags) of
	PFailed l1 l2 err -> do
            throwDyn (InstallationError (showPFailed l1 l2 err))

	POk _ pkg_details -> do
	    return pkg_details
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
