%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

@Uniques@ are used to distinguish entities in the compiler (@Ids@,
@Classes@, etc.) from each other.  Thus, @Uniques@ are the basic
comparison key in the compiler.

If there is any single operation that needs to be fast, it is @Unique@
comparison.  Unsurprisingly, there is quite a bit of huff-and-puff
directed to that end.

Some of the other hair in this code is to be able to use a
``splittable @UniqueSupply@'' if requested/possible (not standard
Haskell).

\begin{code}
module Unique (
	Unique, Uniquable(..), hasKey,

	pprUnique, pprUnique10,

	mkUnique,			-- Used in UniqSupply
	mkUniqueGrimily,		-- Used in UniqSupply only!
	getKey,				-- Used in Var, UniqFM, Name only!

	incrUnique,			-- Used for renumbering
	deriveUnique,			-- Ditto
	newTagUnique,			-- Used in CgCase
	initTyVarUnique,
	initTidyUniques,

	isTupleKey, 

	-- now all the built-in Uniques (and functions to make them)
	-- [the Oh-So-Wonderful Haskell module system wins again...]
	mkAlphaTyVarUnique,
	mkPrimOpIdUnique,
	mkTupleTyConUnique, mkTupleDataConUnique,
	mkPreludeMiscIdUnique, mkPreludeDataConUnique,
	mkPreludeTyConUnique, mkPreludeClassUnique,
	mkPArrDataConUnique,

	mkBuiltinUnique, builtinUniques,
	mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3
    ) where

#include "HsVersions.h"

import BasicTypes	( Boxity(..) )
import FastString	( FastString, uniqueOfFS )
import Outputable
import FastTypes

import GLAEXTS

import Char		( chr, ord )
\end{code}

%************************************************************************
%*									*
\subsection[Unique-type]{@Unique@ type and operations}
%*									*
%************************************************************************

The @Chars@ are ``tag letters'' that identify the @UniqueSupply@.
Fast comparison is everything on @Uniques@:

\begin{code}
data Unique = MkUnique Int#
\end{code}

Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.

\begin{code}
mkUnique	:: Char -> Int -> Unique	-- Builds a unique from pieces
unpkUnique	:: Unique -> (Char, Int)	-- The reverse

mkUniqueGrimily :: Int# -> Unique		-- A trap-door for UniqSupply

getKey		:: Unique -> Int#		-- for Var

incrUnique	:: Unique -> Unique
deriveUnique	:: Unique -> Int -> Unique
newTagUnique	:: Unique -> Char -> Unique

isTupleKey	:: Unique -> Bool
\end{code}


\begin{code}
mkUniqueGrimily x = MkUnique x

{-# INLINE getKey #-}
getKey (MkUnique x) = x

incrUnique (MkUnique i) = MkUnique (i +# 1#)

-- deriveUnique uses an 'X' tag so that it won't clash with
-- any of the uniques produced any other way
deriveUnique (MkUnique i) delta = mkUnique 'X' (I# i + delta)

-- newTagUnique changes the "domain" of a unique to a different char
newTagUnique u c = mkUnique c i where (_,i) = unpkUnique u

-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x::Int#)

mkUnique (C# c) (I# i)
  = MkUnique (w2i (tag `or#` bits))
  where
#if __GLASGOW_HASKELL__ >= 503
    tag  = i2w (ord# c) `uncheckedShiftL#` i2w_s 24#
#else
    tag  = i2w (ord# c) `shiftL#` i2w_s 24#
#endif
    bits = i2w i `and#` (i2w 16777215#){-``0x00ffffff''-}

unpkUnique (MkUnique u)
  = let
	tag = C# (chr# (w2i ((i2w u) `shiftr` (i2w_s 24#))))
	i   = I# (w2i ((i2w u) `and#` (i2w 16777215#){-``0x00ffffff''-}))
    in
    (tag, i)
  where
#if __GLASGOW_HASKELL__ >= 503
    shiftr x y = uncheckedShiftRL# x y
#else
    shiftr x y = shiftRL# x y
#endif
\end{code}



%************************************************************************
%*									*
\subsection[Uniquable-class]{The @Uniquable@ class}
%*									*
%************************************************************************

\begin{code}
class Uniquable a where
    getUnique :: a -> Unique

hasKey		:: Uniquable a => a -> Unique -> Bool
x `hasKey` k	= getUnique x == k

instance Uniquable FastString where
 getUnique fs = mkUniqueGrimily (uniqueOfFS fs)

instance Uniquable Int where
 getUnique (I# i#) = mkUniqueGrimily i#
\end{code}


%************************************************************************
%*									*
\subsection[Unique-instances]{Instance declarations for @Unique@}
%*									*
%************************************************************************

And the whole point (besides uniqueness) is fast equality.  We don't
use `deriving' because we want {\em precise} control of ordering
(equality on @Uniques@ is v common).

\begin{code}
eqUnique (MkUnique u1) (MkUnique u2) = u1 ==# u2
ltUnique (MkUnique u1) (MkUnique u2) = u1 <#  u2
leUnique (MkUnique u1) (MkUnique u2) = u1 <=# u2

cmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 ==# u2 then EQ else if u1 <# u2 then LT else GT

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Ord Unique where
    a  < b = ltUnique a b
    a <= b = leUnique a b
    a  > b = not (leUnique a b)
    a >= b = not (ltUnique a b)
    compare a b = cmpUnique a b

-----------------
instance Uniquable Unique where
    getUnique u = u
\end{code}

We do sometimes make strings with @Uniques@ in them:
\begin{code}
pprUnique, pprUnique10 :: Unique -> SDoc

pprUnique uniq
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (iToBase62 u)

pprUnique10 uniq	-- in base-10, dudes
  = case unpkUnique uniq of
      (tag, u) -> finish_ppr tag u (int u)

finish_ppr 't' u pp_u | u < 26
  =	-- Special case to make v common tyvars, t1, t2, ...
	-- come out as a, b, ... (shorter, easier to read)
    char (chr (ord 'a' + u))
finish_ppr tag u pp_u = char tag <> pp_u

instance Outputable Unique where
    ppr u = pprUnique u

instance Show Unique where
    showsPrec p uniq = showsPrecSDoc p (pprUnique uniq)
\end{code}

%************************************************************************
%*									*
\subsection[Utils-base62]{Base-62 numbers}
%*									*
%************************************************************************

A character-stingy way to read/write numbers (notably Uniques).
The ``62-its'' are \tr{[0-9a-zA-Z]}.  We don't handle negative Ints.
Code stolen from Lennart.

\begin{code}
iToBase62 :: Int -> SDoc

iToBase62 n@(I# n#)
  = ASSERT(n >= 0)
    if n# <# 62# then
	case (indexCharOffAddr# chars62# n#) of { c ->
	char (C# c) }
    else
	case (quotRem n 62)		of { (q, I# r#) ->
	case (indexCharOffAddr# chars62# r#) of { c  ->
	(<>) (iToBase62 q) (char (C# c)) }}
  where
     chars62# = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#
\end{code}

%************************************************************************
%*									*
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
%*									*
%************************************************************************

Allocation of unique supply characters:
	v,t,u : for renumbering value-, type- and usage- vars.
	other a-z: lower case chars for unique supplies (see Main.lhs)
	B:   builtin
	C-E: pseudo uniques	(used in native-code generator)
	X:   uniques derived by deriveUnique
	_:   unifiable tyvars   (above)
	0-9: prelude things below

\begin{code}
mkAlphaTyVarUnique i            = mkUnique '1' i

mkPreludeClassUnique i		= mkUnique '2' i

-- Prelude type constructors occupy *three* slots.
-- The first is for the tycon itself; the latter two
-- are for the generic to/from Ids.  See TysWiredIn.mk_tc_gen_info.

mkPreludeTyConUnique i		= mkUnique '3' (3*i)
mkTupleTyConUnique Boxed   a	= mkUnique '4' (3*a)
mkTupleTyConUnique Unboxed a	= mkUnique '5' (3*a)

-- Data constructor keys occupy *two* slots.  The first is used for the
-- data constructor itself and its wrapper function (the function that
-- evaluates arguments as necessary and calls the worker). The second is
-- used for the worker function (the function that builds the constructor
-- representation).

mkPreludeDataConUnique i	= mkUnique '6' (2*i)	-- Must be alphabetic
mkTupleDataConUnique Boxed a	= mkUnique '7' (2*a)	-- ditto (*may* be used in C labels)
mkTupleDataConUnique Unboxed a	= mkUnique '8' (2*a)

-- This one is used for a tiresome reason
-- to improve a consistency-checking error check in the renamer
isTupleKey u = case unpkUnique u of
		(tag,_) -> tag == '4' || tag == '5' || tag == '7' || tag == '8'

mkPrimOpIdUnique op		= mkUnique '9' op
mkPreludeMiscIdUnique i		= mkUnique '0' i

-- No numbers left anymore, so I pick something different for the character
-- tag 
mkPArrDataConUnique a	        = mkUnique ':' (2*a)

-- The "tyvar uniques" print specially nicely: a, b, c, etc.
-- See pprUnique for details

initTyVarUnique :: Unique
initTyVarUnique = mkUnique 't' 0

initTidyUniques :: (Unique, Unique)	-- Global and local
initTidyUniques = (mkUnique 'g' 0, mkUnique 'x' 0)

mkPseudoUnique1, mkPseudoUnique2, mkPseudoUnique3, 
   mkBuiltinUnique :: Int -> Unique

builtinUniques :: [Unique]
builtinUniques = map mkBuiltinUnique [1..]

mkBuiltinUnique i = mkUnique 'B' i
mkPseudoUnique1 i = mkUnique 'C' i -- used for getUnique on Regs
mkPseudoUnique2 i = mkUnique 'D' i -- used in NCG for getUnique on RealRegs
mkPseudoUnique3 i = mkUnique 'E' i -- used in NCG spiller to create spill VirtualRegs
\end{code}

