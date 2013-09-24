--  Haskell Binding for dl{open,sym,...}          -*-haskell-*-
--
--  Author : Volker Stolz <stolz@i2.informatik.rwth-aachen.de>
--
--  Created: 2001-11-22
--
--  Derived from GModule.chs by M.Weber & M.Chakravarty which is part of c2hs
--  I left the API more or less the same, mostly the flags are different.
--
--  License: BSD
--

module DLPrim (
  dlopen,
  dlsym,
  dlerror,
  dlclose,
  -- dlAddr, -- XXX NYI
  haveRtldNext,
  haveRtldLocal,
  packModuleFlags,
  ModuleFlags(..),
  Source(..)
 )

where

import Data.Bits	( (.|.) )
import Foreign.Ptr	( Ptr, FunPtr, nullPtr, plusPtr )
import Foreign.C.Types	( CInt )
import Foreign.C.String	( CString, withCString, peekCString )

#ifdef HAVE_FRAMEWORK_HASKELLSUPPORT
#include <HaskellSupport/dlfcn.h>
#else
#include <dlfcn.h>
#endif

-- RTLD_NEXT madness
-- On some host (e.g. SuSe Linux 7.2) RTLD_NEXT is not visible
-- without setting _GNU_SOURCE. Since we don't want to set this
-- flag, here's a different solution: You can use the Haskell
-- function 'haveRtldNext' to check wether the flag is available
-- to you. Ideally, this will be optimized by the compiler so
-- that it should be as efficient as an #ifdef.
--    If you fail to test the flag and use it although it is
-- undefined, 'packOneModuleFlag' will bomb.
--    The same applies to RTLD_LOCAL which isn't available on
-- cygwin.

haveRtldNext :: Bool

#ifdef HAVE_RTLDNEXT
haveRtldNext = True

foreign import ccall unsafe rtldNext :: Ptr a
#def inline void *rtldNext (void) {return RTLD_NEXT;} 

#else  /* HAVE_RTLDNEXT */
haveRtldNext = False
#endif /* HAVE_RTLDNEXT */

haveRtldLocal :: Bool

#ifdef HAVE_RTLDLOCAL
haveRtldLocal = True
#else /* HAVE_RTLDLOCAL */
haveRtldLocal = False
#endif /* HAVE_RTLDLOCAL */

-- data type definition
-- --------------------

-- flags passed to `moduleOpen' (EXPORTED)
--
data ModuleFlags 
  = RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL 
  | RTLD_LOCAL
    deriving (Show, Read)

foreign import ccall unsafe "dlopen" dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dlsym"  dlsym  :: (Ptr ()) -> CString -> IO (FunPtr a)
foreign import ccall unsafe "dlerror" dlerror :: IO CString
foreign import ccall unsafe "dlclose" dlclose :: (Ptr ()) -> IO ()

packModuleFlags :: [ModuleFlags] -> CInt
packModuleFlags flags = foldl (\ s f -> (packOneModuleFlag f) .|. s) 0 flags

packOneModuleFlag :: ModuleFlags -> CInt
packOneModuleFlag RTLD_LAZY = #const RTLD_LAZY

#ifdef HAVE_RTLDNOW
packOneModuleFlag RTLD_NOW = #const RTLD_NOW
#else /* HAVE_RTLDNOW */
packOneModuleFlag RTLD_NOW =  error "RTLD_NOW not available"
#endif /* HAVE_RTLDNOW */

#ifdef HAVE_RTLDGLOBAL
packOneModuleFlag RTLD_GLOBAL = #const RTLD_GLOBAL
#else /* HAVE_RTLDGLOBAL */
packOneModuleFlag RTLD_GLOBAL = error "RTLD_GLOBAL not available"
#endif

#ifdef HAVE_RTLDLOCAL
packOneModuleFlag RTLD_LOCAL = #const RTLD_LOCAL
#else /* HAVE_RTLDLOCAL */
packOneModuleFlag RTLD_LOCAL = error "RTLD_LOCAL not available"
#endif /* HAVE_RTLDLOCAL */

data Source = Null | Next | Default | Name CString

packSource :: Source -> CString
packSource Null = nullPtr
#ifdef HAVE_RTLDNEXT
packSource Next = rtldNext
#else
packSource Next = error "RTLD_NEXT not available"
#endif
packSource Default = nullPtr
packSource (Name str) = str
