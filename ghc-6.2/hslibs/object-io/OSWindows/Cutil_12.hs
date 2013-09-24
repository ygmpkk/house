module Cutil_12 (addr2int, int2addr, fpeek, Storable(..), free, malloc,
		 module Data.Int,
		 module Data.Bits,
		 module Foreign.Ptr,
		 module Foreign.C.String) where


{-	This module contains some additional routines required for marshalling 
	Haskell arguments to OS C calling routines.
-}



import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String
import Data.Int
import Data.Bits
import System.IO.Unsafe


--	Conversion operations:

addr2int :: Ptr a -> Int
addr2int a = unsafePerformIO (_casm_GC_ ``%r = (int) %0;'' a)

int2addr :: Int -> Ptr a
int2addr x = unsafePerformIO (_casm_GC_ ``%r = (void *) %0;'' x)
   

--	fpeek addr first peeks addr, then frees addr:
fpeek :: (Storable a) => Ptr a -> IO a
fpeek addr
	= do {
		x <- peek addr;
		free addr;
		return x
	  }