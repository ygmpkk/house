module Win32MM where

import StdDIS
import Win32Types
import GDITypes


copyMemory :: Addr -> Addr -> DWORD -> IO ()
copyMemory arg1 arg2 arg3 =
  prim_copyMemory arg1 arg2 arg3
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_copyMemory" prim_copyMemory :: Addr -> Addr -> Word32 -> IO ()

fillMemory :: Addr -> DWORD -> BYTE -> IO ()
fillMemory arg1 arg2 arg3 =
  prim_fillMemory arg1 arg2 arg3
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_fillMemory" prim_fillMemory :: Addr -> Word32 -> Word8 -> IO ()

getProcessHeap :: IO HANDLE
getProcessHeap =
  prim_getProcessHeap
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_getProcessHeap" prim_getProcessHeap :: IO (Addr)

getProcessHeaps :: DWORD -> Addr -> IO DWORD
getProcessHeaps arg1 arg2 =
  prim_getProcessHeaps arg1 arg2
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_getProcessHeaps" prim_getProcessHeaps :: Word32 -> Addr -> IO (Word32)

type   HGLOBAL   = Addr

type GlobalAllocFlags = UINT


foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_FIXED" gMEM_FIXED :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_MOVEABLE" gMEM_MOVEABLE :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gPTR" gPTR :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gHND" gHND :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_DDESHARE" gMEM_DDESHARE :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_SHARE" gMEM_SHARE :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_LOWER" gMEM_LOWER :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_NOCOMPACT" gMEM_NOCOMPACT :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_NODISCARD" gMEM_NODISCARD :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_NOT_BANKED" gMEM_NOT_BANKED :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_NOTIFY" gMEM_NOTIFY :: GlobalAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_gMEM_ZEROINIT" gMEM_ZEROINIT :: GlobalAllocFlags


globalAlloc :: GlobalAllocFlags -> DWORD -> IO HGLOBAL
globalAlloc arg1 arg2 =
  prim_globalAlloc arg1 arg2
  >>= \ gc_result ->
  access_prim_globalAlloc_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_globalAlloc_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalAlloc_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalAlloc" prim_globalAlloc :: Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalAlloc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalAlloc_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalAlloc_gc_failstring :: Addr -> IO (Addr)

-- %fun GlobalDiscard :: HGLOBAL -> IO HGLOBAL
-- %fail {res1==NULL}{ErrorWin("GlobalDiscard")}

globalFlags :: HGLOBAL -> IO GlobalAllocFlags
globalFlags arg1 =
  prim_globalFlags arg1
  >>= \ gc_result ->
  access_prim_globalFlags_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_globalFlags_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalFlags_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalFlags" prim_globalFlags :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalFlags_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalFlags_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalFlags_gc_failstring :: Addr -> IO (Addr)

globalFree :: HGLOBAL -> IO HGLOBAL
globalFree arg1 =
  prim_globalFree arg1
  >>= \ gc_result ->
  access_prim_globalFree_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_globalFree_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalFree_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalFree" prim_globalFree :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalFree_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalFree_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalFree_gc_failstring :: Addr -> IO (Addr)

globalHandle :: Addr -> IO HGLOBAL
globalHandle arg1 =
  prim_globalHandle arg1
  >>= \ gc_result ->
  access_prim_globalHandle_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_globalHandle_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalHandle_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalHandle" prim_globalHandle :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalHandle_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalHandle_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalHandle_gc_failstring :: Addr -> IO (Addr)

globalLock :: HGLOBAL -> IO Addr
globalLock arg1 =
  prim_globalLock arg1
  >>= \ gc_result ->
  access_prim_globalLock_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_globalLock_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalLock_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalLock" prim_globalLock :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalLock_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalLock_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalLock_gc_failstring :: Addr -> IO (Addr)

-- %fun GlobalMemoryStatus :: IO MEMORYSTATUS

globalReAlloc :: HGLOBAL -> DWORD -> GlobalAllocFlags -> IO HGLOBAL
globalReAlloc arg1 arg2 arg3 =
  prim_globalReAlloc arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_globalReAlloc_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_globalReAlloc_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalReAlloc_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalReAlloc" prim_globalReAlloc :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalReAlloc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalReAlloc_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalReAlloc_gc_failstring :: Addr -> IO (Addr)

globalSize :: HGLOBAL -> IO DWORD
globalSize arg1 =
  prim_globalSize arg1
  >>= \ gc_result ->
  access_prim_globalSize_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_globalSize_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalSize_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalSize" prim_globalSize :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalSize_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalSize_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalSize_gc_failstring :: Addr -> IO (Addr)

globalUnlock :: HGLOBAL -> IO ()
globalUnlock arg1 =
  prim_globalUnlock arg1
  >>= \ gc_result ->
  access_prim_globalUnlock_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_globalUnlock_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_globalUnlock" prim_globalUnlock :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalUnlock_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_globalUnlock_gc_failstring :: Addr -> IO (Addr)

type HeapAllocFlags = DWORD

foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_hEAP_GENERATE_EXCEPTIONS" hEAP_GENERATE_EXCEPTIONS :: HeapAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_hEAP_NO_SERIALIZE" hEAP_NO_SERIALIZE :: HeapAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_hEAP_ZERO_MEMORY" hEAP_ZERO_MEMORY :: HeapAllocFlags

heapAlloc :: HANDLE -> HeapAllocFlags -> DWORD -> IO Addr
heapAlloc arg1 arg2 arg3 =
  prim_heapAlloc arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_heapAlloc_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_heapAlloc_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapAlloc_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapAlloc" prim_heapAlloc :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapAlloc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapAlloc_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapAlloc_gc_failstring :: Addr -> IO (Addr)

heapCompact :: HANDLE -> HeapAllocFlags -> IO UINT
heapCompact arg1 arg2 =
  prim_heapCompact arg1 arg2
  >>= \ gc_result ->
  access_prim_heapCompact_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_heapCompact_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapCompact_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapCompact" prim_heapCompact :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapCompact_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapCompact_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapCompact_gc_failstring :: Addr -> IO (Addr)

heapCreate :: HeapAllocFlags -> DWORD -> DWORD -> IO HANDLE
heapCreate arg1 arg2 arg3 =
  prim_heapCreate arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_heapCreate_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_heapCreate_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapCreate_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapCreate" prim_heapCreate :: Word32 -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapCreate_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapCreate_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapCreate_gc_failstring :: Addr -> IO (Addr)

heapDestroy :: HANDLE -> IO ()
heapDestroy arg1 =
  prim_heapDestroy arg1
  >>= \ gc_result ->
  access_prim_heapDestroy_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapDestroy_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapDestroy" prim_heapDestroy :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapDestroy_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapDestroy_gc_failstring :: Addr -> IO (Addr)

heapFree :: HANDLE -> HeapAllocFlags -> Addr -> IO ()
heapFree arg1 arg2 arg3 =
  prim_heapFree arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_heapFree_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapFree_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapFree" prim_heapFree :: Addr -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapFree_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapFree_gc_failstring :: Addr -> IO (Addr)

heapLock :: HANDLE -> IO ()
heapLock arg1 =
  prim_heapLock arg1
  >>= \ gc_result ->
  access_prim_heapLock_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapLock_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapLock" prim_heapLock :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapLock_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapLock_gc_failstring :: Addr -> IO (Addr)

heapReAlloc :: HANDLE -> HeapAllocFlags -> Addr -> DWORD -> IO Addr
heapReAlloc arg1 arg2 arg3 arg4 =
  prim_heapReAlloc arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_heapReAlloc_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_heapReAlloc_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapReAlloc_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapReAlloc" prim_heapReAlloc :: Addr -> Word32 -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapReAlloc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapReAlloc_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapReAlloc_gc_failstring :: Addr -> IO (Addr)

heapSize :: HANDLE -> HeapAllocFlags -> Addr -> IO DWORD
heapSize arg1 arg2 arg3 =
  prim_heapSize arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_heapSize_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_heapSize_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapSize_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapSize" prim_heapSize :: Addr -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapSize_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapSize_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapSize_gc_failstring :: Addr -> IO (Addr)

heapUnlock :: HANDLE -> IO ()
heapUnlock arg1 =
  prim_heapUnlock arg1
  >>= \ gc_result ->
  access_prim_heapUnlock_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_heapUnlock_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapUnlock" prim_heapUnlock :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapUnlock_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_heapUnlock_gc_failstring :: Addr -> IO (Addr)

heapValidate :: HANDLE -> HeapAllocFlags -> Addr -> IO Bool
heapValidate arg1 arg2 arg3 =
  prim_heapValidate arg1 arg2 arg3
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_heapValidate" prim_heapValidate :: Addr -> Word32 -> Addr -> IO (Int)

moveMemory :: Addr -> Addr -> DWORD -> IO ()
moveMemory arg1 arg2 arg3 =
  prim_moveMemory arg1 arg2 arg3
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_moveMemory" prim_moveMemory :: Addr -> Addr -> Word32 -> IO ()

type VirtualAllocFlags = DWORD

foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_mEM_COMMIT" mEM_COMMIT :: VirtualAllocFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_mEM_RESERVE" mEM_RESERVE :: VirtualAllocFlags

-- % , MEM_TOP_DOWN (not in mingw-20001111 winnt.h)

type ProtectFlags = DWORD

foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_READONLY" pAGE_READONLY :: ProtectFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_READWRITE" pAGE_READWRITE :: ProtectFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_EXECUTE" pAGE_EXECUTE :: ProtectFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_EXECUTE_READ" pAGE_EXECUTE_READ :: ProtectFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_EXECUTE_READWRITE" pAGE_EXECUTE_READWRITE :: ProtectFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_GUARD" pAGE_GUARD :: ProtectFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_NOACCESS" pAGE_NOACCESS :: ProtectFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_pAGE_NOCACHE" pAGE_NOCACHE :: ProtectFlags

type FreeFlags = DWORD

foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_mEM_DECOMMIT" mEM_DECOMMIT :: FreeFlags
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_mEM_RELEASE" mEM_RELEASE :: FreeFlags

virtualAlloc :: Addr -> DWORD -> VirtualAllocFlags -> ProtectFlags -> IO Addr
virtualAlloc arg1 arg2 arg3 arg4 =
  prim_virtualAlloc arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_virtualAlloc_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_virtualAlloc_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_virtualAlloc_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_virtualAlloc" prim_virtualAlloc :: Addr -> Word32 -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualAlloc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualAlloc_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualAlloc_gc_failstring :: Addr -> IO (Addr)


-- %fun VirtualAllocEx :: HANDLE -> Addr -> DWORD -> VirtualAllocFlags -> ProtectFlags ->IO Addr
-- %code extern LPVOID WINAPI VirtualAllocEx(HANDLE,LPVOID,DWORD,DWORD,DWORD);
-- %     LPVOID res1=VirtualAllocEx(arg1,arg2,arg3,arg4,arg5);
-- %fail {res1==NULL}{ErrorWin("VirtualAllocEx")}

virtualFree :: Addr -> DWORD -> FreeFlags -> IO ()
virtualFree arg1 arg2 arg3 =
  prim_virtualFree arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_virtualFree_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_virtualFree_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_virtualFree" prim_virtualFree :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualFree_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualFree_gc_failstring :: Addr -> IO (Addr)

-- %fun VirtualFreeEx :: HANDLE -> Addr -> DWORD -> FreeFlags -> IO ()
-- %code extern BOOL WINAPI VirtualFreeEx(HANDLE,LPVOID,DWORD,DWORD);
-- %     BOOL res1=VirtualFreeEx(arg1,arg2,arg3,arg4);
-- %fail {res1=0}{ErrorWin("VirtualFreeEx")}

virtualLock :: Addr -> DWORD -> IO ()
virtualLock arg1 arg2 =
  prim_virtualLock arg1 arg2
  >>= \ gc_result ->
  access_prim_virtualLock_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_virtualLock_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_virtualLock" prim_virtualLock :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualLock_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualLock_gc_failstring :: Addr -> IO (Addr)

virtualProtect :: Addr -> DWORD -> ProtectFlags -> IO ()
virtualProtect arg1 arg2 arg3 =
  prim_virtualProtect arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_virtualProtect_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_virtualProtect_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_virtualProtect" prim_virtualProtect :: Addr -> Word32 -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualProtect_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualProtect_gc_failstring :: Addr -> IO (Addr)

virtualProtectEx :: HANDLE -> Addr -> DWORD -> ProtectFlags -> Addr -> IO ()
virtualProtectEx arg1 arg2 arg3 arg4 arg5 =
  prim_virtualProtectEx arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_virtualProtectEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_virtualProtectEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_virtualProtectEx" prim_virtualProtectEx :: Addr -> Addr -> Word32 -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualProtectEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualProtectEx_gc_failstring :: Addr -> IO (Addr)

-- No VirtualQuery..()

virtualUnlock :: Addr -> DWORD -> IO ()
virtualUnlock arg1 arg2 =
  prim_virtualUnlock arg1 arg2
  >>= \ gc_result ->
  access_prim_virtualUnlock_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_virtualUnlock_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_virtualUnlock" prim_virtualUnlock :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualUnlock_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32MM_stub_ffi.h" access_prim_virtualUnlock_gc_failstring :: Addr -> IO (Addr)

zeroMemory :: Addr -> DWORD -> IO ()
zeroMemory arg1 arg2 =
  prim_zeroMemory arg1 arg2
foreign import  ccall unsafe "Win32MM_stub_ffi.h prim_zeroMemory" prim_zeroMemory :: Addr -> Word32 -> IO ()

