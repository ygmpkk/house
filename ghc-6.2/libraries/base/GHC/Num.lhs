\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Num
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Num' class and the 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define LEFTMOST_BIT 2147483648
#elif SIZEOF_HSWORD == 8
#define LEFTMOST_BIT 9223372036854775808
#else
#error Please define LEFTMOST_BIT to be 2^(SIZEOF_HSWORD*8-1)
#endif

module GHC.Num where

import {-# SOURCE #-} GHC.Err
import GHC.Base
import GHC.List
import GHC.Enum
import GHC.Show

infixl 7  *
infixl 6  +, -

default ()		-- Double isn't available yet, 
			-- and we shouldn't be using defaults anyway
\end{code}

%*********************************************************
%*							*
\subsection{Standard numeric class}
%*							*
%*********************************************************

\begin{code}
class  (Eq a, Show a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a

    x - y		= x + negate y
    negate x		= 0 - x

{-# INLINE subtract #-}
subtract :: (Num a) => a -> a -> a
subtract x y = y - x
\end{code}


%*********************************************************
%*							*
\subsection{Instances for @Int@}
%*							*
%*********************************************************

\begin{code}
instance  Num Int  where
    (+)	   = plusInt
    (-)	   = minusInt
    negate = negateInt
    (*)	   = timesInt
    abs n  = if n `geInt` 0 then n else negateInt n

    signum n | n `ltInt` 0 = negateInt 1
	     | n `eqInt` 0 = 0
	     | otherwise   = 1

    fromInteger = integer2Int

quotRemInt :: Int -> Int -> (Int, Int)
quotRemInt a@(I# _) b@(I# _) = (a `quotInt` b, a `remInt` b)
    -- OK, so I made it a little stricter.  Shoot me.  (WDP 94/10)

divModInt ::  Int -> Int -> (Int, Int)
divModInt x@(I# _) y@(I# _) = (x `divInt` y, x `modInt` y)
    -- Stricter.  Sorry if you don't like it.  (WDP 94/10)
\end{code}

%*********************************************************
%*							*
\subsection{The @Integer@ type}
%*							*
%*********************************************************

\begin{code}
-- | Arbitrary-precision integers.
data Integer	
   = S# Int#				-- small integers
#ifndef ILX
   | J# Int# ByteArray#			-- large integers
#else
   | J# Void BigInteger                 -- .NET big ints

foreign type dotnet "BigInteger" BigInteger
#endif
\end{code}

Convenient boxed Integer PrimOps. 

\begin{code}
zeroInteger :: Integer
zeroInteger = S# 0#

int2Integer :: Int -> Integer
{-# INLINE int2Integer #-}
int2Integer (I# i) = S# i

integer2Int :: Integer -> Int
integer2Int (S# i)   = I# i
integer2Int (J# s d) = case (integer2Int# s d) of { n# -> I# n# }

toBig (S# i)     = case int2Integer# i of { (# s, d #) -> J# s d }
toBig i@(J# _ _) = i
\end{code}


%*********************************************************
%*							*
\subsection{Dividing @Integers@}
%*							*
%*********************************************************

\begin{code}
quotRemInteger :: Integer -> Integer -> (Integer, Integer)
quotRemInteger a@(S# (-LEFTMOST_BIT#)) b = quotRemInteger (toBig a) b
quotRemInteger (S# i) (S# j)
  = case quotRemInt (I# i) (I# j) of ( I# i, I# j ) -> ( S# i, S# j ) 
quotRemInteger i1@(J# _ _) i2@(S# _) = quotRemInteger i1 (toBig i2)
quotRemInteger i1@(S# _) i2@(J# _ _) = quotRemInteger (toBig i1) i2
quotRemInteger (J# s1 d1) (J# s2 d2)
  = case (quotRemInteger# s1 d1 s2 d2) of
	  (# s3, d3, s4, d4 #)
	    -> (J# s3 d3, J# s4 d4)

divModInteger a@(S# (-LEFTMOST_BIT#)) b = divModInteger (toBig a) b
divModInteger (S# i) (S# j)
  = case divModInt (I# i) (I# j) of ( I# i, I# j ) -> ( S# i, S# j) 
divModInteger i1@(J# _ _) i2@(S# _) = divModInteger i1 (toBig i2)
divModInteger i1@(S# _) i2@(J# _ _) = divModInteger (toBig i1) i2
divModInteger (J# s1 d1) (J# s2 d2)
  = case (divModInteger# s1 d1 s2 d2) of
	  (# s3, d3, s4, d4 #)
	    -> (J# s3 d3, J# s4 d4)

remInteger :: Integer -> Integer -> Integer
remInteger ia 0
  = error "Prelude.Integral.rem{Integer}: divide by 0"
remInteger a@(S# (-LEFTMOST_BIT#)) b = remInteger (toBig a) b
remInteger (S# a) (S# b) = S# (remInt# a b)
{- Special case doesn't work, because a 1-element J# has the range
   -(2^32-1) -- 2^32-1, whereas S# has the range -2^31 -- (2^31-1)
remInteger ia@(S# a) (J# sb b)
  | sb ==# 1#  = S# (remInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (remInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | 0# <# sb   = ia
  | otherwise  = S# (0# -# a)
-}
remInteger ia@(S# _) ib@(J# _ _) = remInteger (toBig ia) ib
remInteger (J# sa a) (S# b)
  = case int2Integer# b of { (# sb, b #) ->
    case remInteger# sa a sb b of { (# sr, r #) ->
    S# (integer2Int# sr r) }}
remInteger (J# sa a) (J# sb b)
  = case remInteger# sa a sb b of (# sr, r #) -> J# sr r

quotInteger :: Integer -> Integer -> Integer
quotInteger ia 0
  = error "Prelude.Integral.quot{Integer}: divide by 0"
quotInteger a@(S# (-LEFTMOST_BIT#)) b = quotInteger (toBig a) b
quotInteger (S# a) (S# b) = S# (quotInt# a b)
{- Special case disabled, see remInteger above
quotInteger (S# a) (J# sb b)
  | sb ==# 1#  = S# (quotInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (quotInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | otherwise  = zeroInteger
-}
quotInteger ia@(S# _) ib@(J# _ _) = quotInteger (toBig ia) ib
quotInteger (J# sa a) (S# b)
  = case int2Integer# b of { (# sb, b #) ->
    case quotInteger# sa a sb b of (# sq, q #) -> J# sq q }
quotInteger (J# sa a) (J# sb b)
  = case quotInteger# sa a sb b of (# sg, g #) -> J# sg g
\end{code}



\begin{code}
gcdInteger :: Integer -> Integer -> Integer
-- SUP: Do we really need the first two cases?
gcdInteger a@(S# (-LEFTMOST_BIT#)) b = gcdInteger (toBig a) b
gcdInteger a b@(S# (-LEFTMOST_BIT#)) = gcdInteger a (toBig b)
gcdInteger (S# a) (S# b) = case gcdInt (I# a) (I# b) of { I# c -> S# c }
gcdInteger ia@(S# 0#) ib@(J# 0# _) = error "GHC.Num.gcdInteger: gcd 0 0 is undefined"
gcdInteger ia@(S# a)  ib@(J# sb b)
  | a  ==# 0#  = abs ib
  | sb ==# 0#  = abs ia
  | otherwise  = S# (gcdIntegerInt# absSb b absA)
       where absA  = if a  <# 0# then negateInt# a  else a
             absSb = if sb <# 0# then negateInt# sb else sb
gcdInteger ia@(J# _ _) ib@(S# _) = gcdInteger ib ia
gcdInteger (J# 0# _) (J# 0# _) = error "GHC.Num.gcdInteger: gcd 0 0 is undefined"
gcdInteger (J# sa a) (J# sb b)
  = case gcdInteger# sa a sb b of (# sg, g #) -> J# sg g

lcmInteger :: Integer -> Integer -> Integer
lcmInteger a 0
  = zeroInteger
lcmInteger 0 b
  = zeroInteger
lcmInteger a b
  = (divExact aa (gcdInteger aa ab)) * ab
  where aa = abs a
        ab = abs b

divExact :: Integer -> Integer -> Integer
divExact a@(S# (-LEFTMOST_BIT#)) b = divExact (toBig a) b
divExact (S# a) (S# b) = S# (quotInt# a b)
divExact (S# a) (J# sb b)
  = S# (quotInt# a (integer2Int# sb b))
divExact (J# sa a) (S# b)
  = case int2Integer# b of
     (# sb, b #) -> case divExactInteger# sa a sb b of (# sd, d #) -> J# sd d
divExact (J# sa a) (J# sb b)
  = case divExactInteger# sa a sb b of (# sd, d #) -> J# sd d
\end{code}


%*********************************************************
%*							*
\subsection{The @Integer@ instances for @Eq@, @Ord@}
%*							*
%*********************************************************

\begin{code}
instance  Eq Integer  where
    (S# i)     ==  (S# j)     = i ==# j
    (S# i)     ==  (J# s d)   = cmpIntegerInt# s d i ==# 0#
    (J# s d)   ==  (S# i)     = cmpIntegerInt# s d i ==# 0#
    (J# s1 d1) ==  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ==# 0#

    (S# i)     /=  (S# j)     = i /=# j
    (S# i)     /=  (J# s d)   = cmpIntegerInt# s d i /=# 0#
    (J# s d)   /=  (S# i)     = cmpIntegerInt# s d i /=# 0#
    (J# s1 d1) /=  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) /=# 0#

------------------------------------------------------------------------
instance  Ord Integer  where
    (S# i)     <=  (S# j)     = i <=# j
    (J# s d)   <=  (S# i)     = cmpIntegerInt# s d i <=# 0#
    (S# i)     <=  (J# s d)   = cmpIntegerInt# s d i >=# 0#
    (J# s1 d1) <=  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <=# 0#

    (S# i)     >   (S# j)     = i ># j
    (J# s d)   >   (S# i)     = cmpIntegerInt# s d i ># 0#
    (S# i)     >   (J# s d)   = cmpIntegerInt# s d i <# 0#
    (J# s1 d1) >   (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ># 0#

    (S# i)     <   (S# j)     = i <# j
    (J# s d)   <   (S# i)     = cmpIntegerInt# s d i <# 0#
    (S# i)     <   (J# s d)   = cmpIntegerInt# s d i ># 0#
    (J# s1 d1) <   (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <# 0#

    (S# i)     >=  (S# j)     = i >=# j
    (J# s d)   >=  (S# i)     = cmpIntegerInt# s d i >=# 0#
    (S# i)     >=  (J# s d)   = cmpIntegerInt# s d i <=# 0#
    (J# s1 d1) >=  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) >=# 0#

    compare (S# i)  (S# j)
       | i ==# j = EQ
       | i <=# j = LT
       | otherwise = GT
    compare (J# s d) (S# i)
       = case cmpIntegerInt# s d i of { res# ->
	 if res# <# 0# then LT else 
	 if res# ># 0# then GT else EQ
	 }
    compare (S# i) (J# s d)
       = case cmpIntegerInt# s d i of { res# ->
	 if res# ># 0# then LT else 
	 if res# <# 0# then GT else EQ
	 }
    compare (J# s1 d1) (J# s2 d2)
       = case cmpInteger# s1 d1 s2 d2 of { res# ->
	 if res# <# 0# then LT else 
	 if res# ># 0# then GT else EQ
	 }
\end{code}


%*********************************************************
%*							*
\subsection{The @Integer@ instances for @Num@}
%*							*
%*********************************************************

\begin{code}
instance  Num Integer  where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    negate	   = negateInteger
    fromInteger	x  =  x

    -- ORIG: abs n = if n >= 0 then n else -n
    abs (S# (-LEFTMOST_BIT#)) = LEFTMOST_BIT
    abs (S# i) = case abs (I# i) of I# j -> S# j
    abs n@(J# s d) = if (s >=# 0#) then n else J# (negateInt# s) d

    signum (S# i) = case signum (I# i) of I# j -> S# j
    signum (J# s d)
      = let
	    cmp = cmpIntegerInt# s d 0#
	in
	if      cmp >#  0# then S# 1#
	else if cmp ==# 0# then S# 0#
	else			S# (negateInt# 1#)

plusInteger i1@(S# i) i2@(S# j)  = case addIntC# i j of { (# r, c #) ->
				   if c ==# 0# then S# r
				   else toBig i1 + toBig i2 }
plusInteger i1@(J# _ _) i2@(S# _) = i1 + toBig i2
plusInteger i1@(S# _) i2@(J# _ _) = toBig i1 + i2
plusInteger (J# s1 d1) (J# s2 d2) = case plusInteger# s1 d1 s2 d2 of (# s, d #) -> J# s d

minusInteger i1@(S# i) i2@(S# j)   = case subIntC# i j of { (# r, c #) ->
				     if c ==# 0# then S# r
				     else toBig i1 - toBig i2 }
minusInteger i1@(J# _ _) i2@(S# _) = i1 - toBig i2
minusInteger i1@(S# _) i2@(J# _ _) = toBig i1 - i2
minusInteger (J# s1 d1) (J# s2 d2) = case minusInteger# s1 d1 s2 d2 of (# s, d #) -> J# s d

timesInteger i1@(S# i) i2@(S# j)   = if   mulIntMayOflo# i j ==# 0#
                                     then S# (i *# j)
                                     else toBig i1 * toBig i2 
timesInteger i1@(J# _ _) i2@(S# _) = i1 * toBig i2
timesInteger i1@(S# _) i2@(J# _ _) = toBig i1 * i2
timesInteger (J# s1 d1) (J# s2 d2) = case timesInteger# s1 d1 s2 d2 of (# s, d #) -> J# s d

negateInteger (S# (-LEFTMOST_BIT#)) = LEFTMOST_BIT
negateInteger (S# i)		  = S# (negateInt# i)
negateInteger (J# s d)		  = J# (negateInt# s) d
\end{code}


%*********************************************************
%*							*
\subsection{The @Integer@ instance for @Enum@}
%*							*
%*********************************************************

\begin{code}
instance  Enum Integer  where
    succ x		 = x + 1
    pred x		 = x - 1
    toEnum n		 = int2Integer n
    fromEnum n		 = integer2Int n

    {-# INLINE enumFrom #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromTo #-}
    {-# INLINE enumFromThenTo #-}
    enumFrom x             = enumDeltaInteger  x 1
    enumFromThen x y       = enumDeltaInteger  x (y-x)
    enumFromTo x lim	   = enumDeltaToInteger x 1     lim
    enumFromThenTo x y lim = enumDeltaToInteger x (y-x) lim

{-# RULES
"enumDeltaInteger"	[~1] forall x y.  enumDeltaInteger x y     = build (\c _ -> enumDeltaIntegerFB c x y)
"efdtInteger"		[~1] forall x y l.enumDeltaToInteger x y l = build (\c n -> enumDeltaToIntegerFB c n x y l)
"enumDeltaInteger" 	[1] enumDeltaIntegerFB   (:)    = enumDeltaInteger
"enumDeltaToInteger" 	[1] enumDeltaToIntegerFB (:) [] = enumDeltaToInteger
 #-}

enumDeltaIntegerFB :: (Integer -> b -> b) -> Integer -> Integer -> b
enumDeltaIntegerFB c x d = x `c` enumDeltaIntegerFB c (x+d) d

enumDeltaInteger :: Integer -> Integer -> [Integer]
enumDeltaInteger x d = x : enumDeltaInteger (x+d) d

enumDeltaToIntegerFB c n x delta lim
  | delta >= 0 = up_fb c n x delta lim
  | otherwise  = dn_fb c n x delta lim

enumDeltaToInteger x delta lim
  | delta >= 0 = up_list x delta lim
  | otherwise  = dn_list x delta lim

up_fb c n x delta lim = go (x::Integer)
		      where
			go x | x > lim   = n
			     | otherwise = x `c` go (x+delta)
dn_fb c n x delta lim = go (x::Integer)
		      where
			go x | x < lim   = n
			     | otherwise = x `c` go (x+delta)

up_list x delta lim = go (x::Integer)
		    where
			go x | x > lim   = []
			     | otherwise = x : go (x+delta)
dn_list x delta lim = go (x::Integer)
		    where
			go x | x < lim   = []
			     | otherwise = x : go (x+delta)

\end{code}


%*********************************************************
%*							*
\subsection{The @Integer@ instances for @Show@}
%*							*
%*********************************************************

\begin{code}
instance Show Integer where
    showsPrec p n r
        | p > 6 && n < 0 = '(' : jtos n (')' : r)
		-- Minor point: testing p first gives better code 
		-- in the not-uncommon case where the p argument
		-- is a constant
        | otherwise      = jtos n r
    showList = showList__ (showsPrec 0)

jtos :: Integer -> String -> String
jtos n cs
    | n < 0     = '-' : jtos' (-n) cs
    | otherwise = jtos' n cs
    where
    jtos' :: Integer -> String -> String
    jtos' n' cs'
        | n' < 10    = case unsafeChr (ord '0' + fromInteger n') of
            c@(C# _) -> c:cs'
        | otherwise = case unsafeChr (ord '0' + fromInteger r) of
            c@(C# _) -> jtos' q (c:cs')
        where
        (q,r) = n' `quotRemInteger` 10
\end{code}
