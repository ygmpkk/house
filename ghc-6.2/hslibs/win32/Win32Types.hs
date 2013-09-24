module Win32Types where

import StdDIS

----------------------------------------------------------------
-- Platform specific definitions
--
-- Most typedefs and prototypes in Win32 are expressed in terms
-- of these types.  Try to follow suit - it'll make it easier to
-- get things working on Win64 (or whatever they call it on Alphas).
----------------------------------------------------------------

	           
type BOOL          = Bool
type BYTE          = Word8
type USHORT        = Word16
type UINT          = Word32
type INT           = Int32
type WORD          = Word16
type DWORD         = Word32
type LONG          = Int32
type FLOAT         = Float
	           
type MbINT         = Maybe INT

----------------------------------------------------------------

	           
type ATOM          = UINT
type WPARAM        = UINT
type LPARAM        = LONG
type LRESULT       = LONG
	           
type MbATOM        = Maybe ATOM

----------------------------------------------------------------
-- Pointers
----------------------------------------------------------------

type LPVOID        = Addr
type LPCTSTR       = Addr
type LPCTSTR_      = String
type LPCSTR        = Addr
type LPSTR         = Addr



-- Note: marshalling allocates mem, so the programmer
-- has to make sure to free this stuff up after any
-- uses of LPCTSTR. Automating this is tricky to do
-- (in all situations).

unmarshall_lpctstr_ :: Addr -> IO String
unmarshall_lpctstr_ arg1 =
  prim_unmarshall_lpctstr_ arg1
  >>= \ gc_result ->
  access_prim_unmarshall_lpctstr__gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_unmarshall_lpctstr__gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_unmarshall_lpctstr__gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res2) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32Types_stub_ffi.h prim_unmarshall_lpctstr_" prim_unmarshall_lpctstr_ :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Types_stub_ffi.h" access_prim_unmarshall_lpctstr__gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Types_stub_ffi.h" access_prim_unmarshall_lpctstr__gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Types_stub_ffi.h" access_prim_unmarshall_lpctstr__gc_failstring :: Addr -> IO (Addr)

marshall_lpctstr_ :: String -> IO Addr
marshall_lpctstr_ gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_marshall_lpctstr_ arg1
  >>= \ gc_result ->
  access_prim_marshall_lpctstr__res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_marshall_lpctstr__gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  if ( gc_failed /= (0::Int))
  then 
      access_prim_marshall_lpctstr__gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
      unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32Types_stub_ffi.h prim_marshall_lpctstr_" prim_marshall_lpctstr_ :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Types_stub_ffi.h" access_prim_marshall_lpctstr__res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Types_stub_ffi.h" access_prim_marshall_lpctstr__gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Types_stub_ffi.h" access_prim_marshall_lpctstr__gc_failstring :: Addr -> IO (Addr)

type MbLPVOID      = Maybe LPVOID
		   
type MbLPCSTR      = Maybe LPCSTR
type MbLPCTSTR     = Maybe LPCTSTR
		   
----------------------------------------------------------------
-- Handles
----------------------------------------------------------------

type   HANDLE      = Addr
foreign import  ccall unsafe "Win32Types_stub_ffi.h prim_handleToWord" handleToWord :: HANDLE -> UINT
type   HKEY      = ForeignPtr ()
		   
foreign import  ccall unsafe "Win32Types_stub_ffi.h prim_nullHANDLE" nullHANDLE :: Addr

type MbHANDLE      = Maybe HANDLE

type   HINSTANCE   = Addr
type MbHINSTANCE   = Maybe HINSTANCE

type   HMODULE     = Addr
type MbHMODULE     = Maybe HMODULE

nullFinalHANDLE :: ForeignPtr ()
nullFinalHANDLE = unsafePerformIO (makeForeignPtr nullAddr nullAddr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
