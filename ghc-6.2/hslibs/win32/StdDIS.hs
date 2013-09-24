--
-- StdDIS for GHC
--
-- (c) Thomas Nordin and Alastair Reid, 1997
--

module StdDIS
        ( StablePtr
        , ForeignObj
        , module Int
        , module Word
        , module Addr
        , module IOExts
        , MbString
        , marshall_bool_,      unmarshall_bool_
        , marshall_string_,    unmarshall_string_
        , marshall_stringLen_, unmarshall_stringLen_
        , makeStablePtr, deRefStablePtr, freeStablePtr
        , malloc, free
        , makeForeignPtr
	, ForeignPtr
	, Ptr
	, withForeignPtr

	   -- re-exporting base Prelude types
	   -- (useful when generating source that
	   --  import StdDIS qualified.)
	, Float
	, Double
	, Word
	, Int
	, Char
	, FunPtr
        ) where


import Int
import Word
import Addr
import IOExts
import Foreign ( StablePtr, newStablePtr,
                 deRefStablePtr, 
                 freeStablePtr,
		 FunPtr
	       )
import ForeignObj ( ForeignObj, mkForeignObj, addForeignFinalizer )
import CString
import qualified MarshalAlloc
import Foreign.ForeignPtr
import GHC.Ptr

#if __GLASGOW_HASKELL__ <= 502
import PrelPtr

ptrToAddr :: Ptr a -> Addr
ptrToAddr (Ptr x) = A# x 

addrToPtr :: Addr -> Ptr a
addrToPtr (A# x) = Ptr x

#endif

makeForeignPtr :: Addr{-x-} -> Addr{-free-} -> IO (ForeignPtr ())
makeForeignPtr addr finAddr = newForeignPtr (castPtrToFunPtr (addrToPtr finAddr)) (addrToPtr addr) 

makeStablePtr = newStablePtr







marshall_bool_ :: Bool -> IO Int
marshall_bool_ True  = return 1
marshall_bool_ False = return 0

unmarshall_bool_ :: Int -> IO Bool
unmarshall_bool_ 0 = return False
unmarshall_bool_ _ = return True

-- Ignore "IO" part of result type

----------------------------------------------------------------
-- Strings
----------------------------------------------------------------


type MbString      = Maybe String

marshall_string_ :: [Char] -> IO Addr
marshall_string_ cs = do
    pcs <- newCString cs
    return (ptrToAddr pcs)

marshall_stringLen_ :: [Char] -> IO (Addr, Int)
marshall_stringLen_ cs = do
    (pcs,len) <- newCStringLen cs
    return (ptrToAddr pcs, len)

unmarshall_string_ :: Addr -> IO String
unmarshall_string_ ptr = peekCString (addrToPtr ptr)

unmarshall_stringLen_ :: Addr -> Int -> IO String
unmarshall_stringLen_ ptr l = peekCStringLen (addrToPtr ptr, l)

----------------------------------------------------------------
-- malloc/free
----------------------------------------------------------------

malloc :: Word32 -> IO Addr
malloc sz = MarshalAlloc.mallocBytes (fromIntegral sz) >>= return.ptrToAddr

free :: Addr -> IO ()
free p = MarshalAlloc.free (addrToPtr p)
 
-- what does this actually do?

----------------------------------------------------------------
-- Stable pointers
----------------------------------------------------------------

--
-- Use "stable" to create a stable pointer
-- 
-- Use "stablePtr" to manipulate (previously constructed) stable pointers 
-- in Haskell.
--


----------------------------------------------------------------
-- Foreign Objects
----------------------------------------------------------------

-- %foreign will convert an Addr into a ForeignObj


----------------------------------------------------------------
-- End of StdDIS
----------------------------------------------------------------
