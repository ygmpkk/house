{-# OPTIONS -cpp #-}

-- #hide
-----------------------------------------------------------------------------
-- Module      :  OS.Cutil_12
-- Copyright   :  (c) Krasimir Andreev 2002
-- License     :  BSD-style
-- 
-- Maintainer  :  ka2_mail@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains some additional routines required for marshalling 
-- Haskell arguments to OS C calling routines.
--
-----------------------------------------------------------------------------

module Graphics.UI.ObjectIO.OS.Cutil_12
		( addr2int, int2addr, fpeek, Storable(..), free, malloc
		, module Data.Int
		, module Data.Bits
		, module Foreign.Ptr
		, module Foreign.C.String
		) where


import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String
import Data.Int
import Data.Bits
import System.IO.Unsafe


--	Conversion operations:

#ifndef __HADDOCK__
addr2int :: Ptr a -> Int
addr2int a = unsafePerformIO (_casm_GC_ ``%r = (int) %0;'' a)

int2addr :: Int -> Ptr a
int2addr x = unsafePerformIO (_casm_GC_ ``%r = (void *) %0;'' x)
#endif


--	fpeek addr first peeks addr, then frees addr:
fpeek :: (Storable a) => Ptr a -> IO a
fpeek addr
	= do {
		x <- peek addr;
		free addr;
		return x
	  }