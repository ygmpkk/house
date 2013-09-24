module Win32File
{-
	( AccessMode, ShareMode, CreateMode, FileAttributeOrFlag
	, CreateFile, CloseHandle, DeleteFile, CopyFile
	, MoveFileFlag, MoveFile, MoveFileEx, 
	)
-}
where

import Win32Types
import StdDIS


----------------------------------------------------------------
-- Enumeration types
----------------------------------------------------------------

type AccessMode   = UINT

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_gENERIC_NONE" gENERIC_NONE :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_gENERIC_READ" gENERIC_READ :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_gENERIC_WRITE" gENERIC_WRITE :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_gENERIC_EXECUTE" gENERIC_EXECUTE :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_gENERIC_ALL" gENERIC_ALL :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dELETE" dELETE :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_rEAD_CONTROL" rEAD_CONTROL :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_wRITE_DAC" wRITE_DAC :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_wRITE_OWNER" wRITE_OWNER :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sYNCHRONIZE" sYNCHRONIZE :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sTANDARD_RIGHTS_REQUIRED" sTANDARD_RIGHTS_REQUIRED :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sTANDARD_RIGHTS_READ" sTANDARD_RIGHTS_READ :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sTANDARD_RIGHTS_WRITE" sTANDARD_RIGHTS_WRITE :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sTANDARD_RIGHTS_EXECUTE" sTANDARD_RIGHTS_EXECUTE :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sTANDARD_RIGHTS_ALL" sTANDARD_RIGHTS_ALL :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sPECIFIC_RIGHTS_ALL" sPECIFIC_RIGHTS_ALL :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_aCCESS_SYSTEM_SECURITY" aCCESS_SYSTEM_SECURITY :: AccessMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_mAXIMUM_ALLOWED" mAXIMUM_ALLOWED :: AccessMode

----------------------------------------------------------------

type ShareMode   = UINT

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_SHARE_NONE" fILE_SHARE_NONE :: ShareMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_SHARE_READ" fILE_SHARE_READ :: ShareMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_SHARE_WRITE" fILE_SHARE_WRITE :: ShareMode

----------------------------------------------------------------

type CreateMode   = UINT

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_cREATE_NEW" cREATE_NEW :: CreateMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_cREATE_ALWAYS" cREATE_ALWAYS :: CreateMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_oPEN_EXISTING" oPEN_EXISTING :: CreateMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_oPEN_ALWAYS" oPEN_ALWAYS :: CreateMode
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_tRUNCATE_EXISTING" tRUNCATE_EXISTING :: CreateMode

----------------------------------------------------------------

type FileAttributeOrFlag   = UINT

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_READONLY" fILE_ATTRIBUTE_READONLY :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_HIDDEN" fILE_ATTRIBUTE_HIDDEN :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_SYSTEM" fILE_ATTRIBUTE_SYSTEM :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_DIRECTORY" fILE_ATTRIBUTE_DIRECTORY :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_ARCHIVE" fILE_ATTRIBUTE_ARCHIVE :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_NORMAL" fILE_ATTRIBUTE_NORMAL :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_TEMPORARY" fILE_ATTRIBUTE_TEMPORARY :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_ATTRIBUTE_COMPRESSED" fILE_ATTRIBUTE_COMPRESSED :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_WRITE_THROUGH" fILE_FLAG_WRITE_THROUGH :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_OVERLAPPED" fILE_FLAG_OVERLAPPED :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_NO_BUFFERING" fILE_FLAG_NO_BUFFERING :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_RANDOM_ACCESS" fILE_FLAG_RANDOM_ACCESS :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_SEQUENTIAL_SCAN" fILE_FLAG_SEQUENTIAL_SCAN :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_DELETE_ON_CLOSE" fILE_FLAG_DELETE_ON_CLOSE :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_BACKUP_SEMANTICS" fILE_FLAG_BACKUP_SEMANTICS :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_FLAG_POSIX_SEMANTICS" fILE_FLAG_POSIX_SEMANTICS :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_ANONYMOUS" sECURITY_ANONYMOUS :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_IDENTIFICATION" sECURITY_IDENTIFICATION :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_IMPERSONATION" sECURITY_IMPERSONATION :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_DELEGATION" sECURITY_DELEGATION :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_CONTEXT_TRACKING" sECURITY_CONTEXT_TRACKING :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_EFFECTIVE_ONLY" sECURITY_EFFECTIVE_ONLY :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_SQOS_PRESENT" sECURITY_SQOS_PRESENT :: FileAttributeOrFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sECURITY_VALID_SQOS_FLAGS" sECURITY_VALID_SQOS_FLAGS :: FileAttributeOrFlag

----------------------------------------------------------------

type MoveFileFlag   = DWORD

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_mOVEFILE_REPLACE_EXISTING" mOVEFILE_REPLACE_EXISTING :: MoveFileFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_mOVEFILE_COPY_ALLOWED" mOVEFILE_COPY_ALLOWED :: MoveFileFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_mOVEFILE_DELAY_UNTIL_REBOOT" mOVEFILE_DELAY_UNTIL_REBOOT :: MoveFileFlag

----------------------------------------------------------------

type FilePtrDirection   = DWORD

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_BEGIN" fILE_BEGIN :: FilePtrDirection
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_CURRENT" fILE_CURRENT :: FilePtrDirection
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_END" fILE_END :: FilePtrDirection

----------------------------------------------------------------

type DriveType = UINT

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dRIVE_UNKNOWN" dRIVE_UNKNOWN :: DriveType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dRIVE_NO_ROOT_DIR" dRIVE_NO_ROOT_DIR :: DriveType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dRIVE_REMOVABLE" dRIVE_REMOVABLE :: DriveType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dRIVE_FIXED" dRIVE_FIXED :: DriveType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dRIVE_REMOTE" dRIVE_REMOTE :: DriveType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dRIVE_CDROM" dRIVE_CDROM :: DriveType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dRIVE_RAMDISK" dRIVE_RAMDISK :: DriveType
----------------------------------------------------------------

type DefineDosDeviceFlags = DWORD

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dDD_RAW_TARGET_PATH" dDD_RAW_TARGET_PATH :: DefineDosDeviceFlags
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dDD_REMOVE_DEFINITION" dDD_REMOVE_DEFINITION :: DefineDosDeviceFlags
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_dDD_EXACT_MATCH_ON_REMOVE" dDD_EXACT_MATCH_ON_REMOVE :: DefineDosDeviceFlags

----------------------------------------------------------------

type BinaryType = DWORD

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sCS_32BIT_BINARY" sCS_32BIT_BINARY :: BinaryType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sCS_DOS_BINARY" sCS_DOS_BINARY :: BinaryType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sCS_WOW_BINARY" sCS_WOW_BINARY :: BinaryType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sCS_PIF_BINARY" sCS_PIF_BINARY :: BinaryType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sCS_POSIX_BINARY" sCS_POSIX_BINARY :: BinaryType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_sCS_OS216_BINARY" sCS_OS216_BINARY :: BinaryType

----------------------------------------------------------------

type FileNotificationFlag = DWORD

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_NOTIFY_CHANGE_FILE_NAME" fILE_NOTIFY_CHANGE_FILE_NAME :: FileNotificationFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_NOTIFY_CHANGE_DIR_NAME" fILE_NOTIFY_CHANGE_DIR_NAME :: FileNotificationFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_NOTIFY_CHANGE_ATTRIBUTES" fILE_NOTIFY_CHANGE_ATTRIBUTES :: FileNotificationFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_NOTIFY_CHANGE_SIZE" fILE_NOTIFY_CHANGE_SIZE :: FileNotificationFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_NOTIFY_CHANGE_LAST_WRITE" fILE_NOTIFY_CHANGE_LAST_WRITE :: FileNotificationFlag
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_NOTIFY_CHANGE_SECURITY" fILE_NOTIFY_CHANGE_SECURITY :: FileNotificationFlag

----------------------------------------------------------------

type FileType = DWORD

foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_TYPE_UNKNOWN" fILE_TYPE_UNKNOWN :: FileType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_TYPE_DISK" fILE_TYPE_DISK :: FileType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_TYPE_CHAR" fILE_TYPE_CHAR :: FileType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_TYPE_PIPE" fILE_TYPE_PIPE :: FileType
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_fILE_TYPE_REMOTE" fILE_TYPE_REMOTE :: FileType

----------------------------------------------------------------

type LPSECURITY_ATTRIBUTES = Addr
type MbLPSECURITY_ATTRIBUTES = Maybe LPSECURITY_ATTRIBUTES

----------------------------------------------------------------
-- File operations
----------------------------------------------------------------

deleteFile :: String -> IO ()
deleteFile gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_deleteFile arg1
  >>= \ gc_result ->
  access_prim_deleteFile_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_deleteFile_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_deleteFile" prim_deleteFile :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_deleteFile_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_deleteFile_gc_failstring :: Addr -> IO (Addr)

copyFile :: String -> String -> Bool -> IO ()
copyFile gc_arg1 gc_arg2 gc_arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_string_ gc_arg2) >>= \ (arg2) ->
  (marshall_bool_ gc_arg3) >>= \ (arg3) ->
  prim_copyFile arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_copyFile_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_copyFile_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_copyFile" prim_copyFile :: Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_copyFile_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_copyFile_gc_failstring :: Addr -> IO (Addr)

moveFile :: String -> String -> IO ()
moveFile gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_string_ gc_arg2) >>= \ (arg2) ->
  prim_moveFile arg1 arg2
  >>= \ gc_result ->
  access_prim_moveFile_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_moveFile_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_moveFile" prim_moveFile :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_moveFile_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_moveFile_gc_failstring :: Addr -> IO (Addr)

moveFileEx :: String -> String -> MoveFileFlag -> IO ()
moveFileEx gc_arg1 gc_arg2 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_string_ gc_arg2) >>= \ (arg2) ->
  prim_moveFileEx arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_moveFileEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_moveFileEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_moveFileEx" prim_moveFileEx :: Addr -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_moveFileEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_moveFileEx_gc_failstring :: Addr -> IO (Addr)

setCurrentDirectory :: String -> IO ()
setCurrentDirectory gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_setCurrentDirectory arg1
  >>= \ gc_result ->
  access_prim_setCurrentDirectory_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setCurrentDirectory_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_setCurrentDirectory" prim_setCurrentDirectory :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setCurrentDirectory_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setCurrentDirectory_gc_failstring :: Addr -> IO (Addr)

createDirectory :: String -> MbLPSECURITY_ATTRIBUTES -> IO ()
createDirectory gc_arg1 arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (case arg2 of {
      Nothing -> (return (nullAddr));
      (Just arg2) -> (return ((arg2)))
   }) >>= \ (arg2) ->
  prim_createDirectory arg1 arg2
  >>= \ gc_result ->
  access_prim_createDirectory_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDirectory_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_createDirectory" prim_createDirectory :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_createDirectory_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_createDirectory_gc_failstring :: Addr -> IO (Addr)

createDirectoryEx :: String -> String -> MbLPSECURITY_ATTRIBUTES -> IO ()
createDirectoryEx gc_arg1 gc_arg2 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_string_ gc_arg2) >>= \ (arg2) ->
  (case arg3 of {
      Nothing -> (return (nullAddr));
      (Just arg3) -> (return ((arg3)))
   }) >>= \ (arg3) ->
  prim_createDirectoryEx arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_createDirectoryEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createDirectoryEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_createDirectoryEx" prim_createDirectoryEx :: Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_createDirectoryEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_createDirectoryEx_gc_failstring :: Addr -> IO (Addr)

removeDirectory :: String -> IO ()
removeDirectory gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_removeDirectory arg1
  >>= \ gc_result ->
  access_prim_removeDirectory_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_removeDirectory_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_removeDirectory" prim_removeDirectory :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_removeDirectory_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_removeDirectory_gc_failstring :: Addr -> IO (Addr)

getBinaryType :: String -> IO BinaryType
getBinaryType gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_getBinaryType arg1
  >>= \ gc_result ->
  access_prim_getBinaryType_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getBinaryType_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getBinaryType_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_getBinaryType" prim_getBinaryType :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getBinaryType_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getBinaryType_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getBinaryType_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- HANDLE operations
----------------------------------------------------------------

createFile :: String -> AccessMode -> ShareMode -> MbLPSECURITY_ATTRIBUTES -> CreateMode -> FileAttributeOrFlag -> MbHANDLE -> IO HANDLE
createFile gc_arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (case arg4 of {
      Nothing -> (return (nullAddr));
      (Just arg4) -> (return ((arg4)))
   }) >>= \ (arg4) ->
  (case arg7 of {
      Nothing -> (return (nullHANDLE));
      (Just arg7) -> (return ((arg7)))
   }) >>= \ (arg7) ->
  prim_createFile arg1 arg2 arg3 arg4 arg5 arg6 arg7
  >>= \ gc_result ->
  access_prim_createFile_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_createFile_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_createFile_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_createFile" prim_createFile :: Addr -> Word32 -> Word32 -> Addr -> Word32 -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_createFile_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_createFile_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_createFile_gc_failstring :: Addr -> IO (Addr)

closeHandle :: HANDLE -> IO ()
closeHandle arg1 =
  prim_closeHandle arg1
  >>= \ gc_result ->
  access_prim_closeHandle_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_closeHandle_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_closeHandle" prim_closeHandle :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_closeHandle_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_closeHandle_gc_failstring :: Addr -> IO (Addr)

getFileType :: HANDLE -> IO FileType
getFileType arg1 =
  prim_getFileType arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_getFileType" prim_getFileType :: Addr -> IO (Word32)
--Apparently no error code

flushFileBuffers :: HANDLE -> IO ()
flushFileBuffers arg1 =
  prim_flushFileBuffers arg1
  >>= \ gc_result ->
  access_prim_flushFileBuffers_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_flushFileBuffers_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_flushFileBuffers" prim_flushFileBuffers :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_flushFileBuffers_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_flushFileBuffers_gc_failstring :: Addr -> IO (Addr)

setEndOfFile :: HANDLE -> IO ()
setEndOfFile arg1 =
  prim_setEndOfFile arg1
  >>= \ gc_result ->
  access_prim_setEndOfFile_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setEndOfFile_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_setEndOfFile" prim_setEndOfFile :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setEndOfFile_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setEndOfFile_gc_failstring :: Addr -> IO (Addr)

setFileAttributes :: String -> FileAttributeOrFlag -> IO ()
setFileAttributes gc_arg1 arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_setFileAttributes arg1 arg2
  >>= \ gc_result ->
  access_prim_setFileAttributes_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setFileAttributes_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_setFileAttributes" prim_setFileAttributes :: Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setFileAttributes_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setFileAttributes_gc_failstring :: Addr -> IO (Addr)

getFileAttributes :: String -> IO FileAttributeOrFlag
getFileAttributes gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_getFileAttributes arg1
  >>= \ gc_result ->
  access_prim_getFileAttributes_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getFileAttributes_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getFileAttributes_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_getFileAttributes" prim_getFileAttributes :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getFileAttributes_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getFileAttributes_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getFileAttributes_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- Read/write files
----------------------------------------------------------------

-- No support for this yet
--type OVERLAPPED =
-- (DWORD,  -- Offset
--  DWORD,  -- OffsetHigh
--  HANDLE) -- hEvent

type LPOVERLAPPED = Addr

type MbLPOVERLAPPED = Maybe LPOVERLAPPED

--Sigh - I give up & prefix win32_ to the next two to avoid
-- senseless Prelude name clashes. --sof.

win32_ReadFile :: HANDLE -> Addr -> DWORD -> MbLPOVERLAPPED -> IO DWORD
win32_ReadFile arg1 arg2 arg3 arg4 =
  (case arg4 of {
      Nothing -> (return (nullAddr));
      (Just arg4) -> (return ((arg4)))
   }) >>= \ (arg4) ->
  prim_win32_ReadFile arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_win32_ReadFile_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_win32_ReadFile_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_win32_ReadFile_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_win32_ReadFile" prim_win32_ReadFile :: Addr -> Addr -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_win32_ReadFile_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_win32_ReadFile_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_win32_ReadFile_gc_failstring :: Addr -> IO (Addr)

win32_WriteFile :: HANDLE -> Addr -> DWORD -> MbLPOVERLAPPED -> IO DWORD
win32_WriteFile arg1 arg2 arg3 arg4 =
  (case arg4 of {
      Nothing -> (return (nullAddr));
      (Just arg4) -> (return ((arg4)))
   }) >>= \ (arg4) ->
  prim_win32_WriteFile arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_win32_WriteFile_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_win32_WriteFile_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_win32_WriteFile_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_win32_WriteFile" prim_win32_WriteFile :: Addr -> Addr -> Word32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_win32_WriteFile_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_win32_WriteFile_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_win32_WriteFile_gc_failstring :: Addr -> IO (Addr)

-- missing Seek functioinality; GSL ???
-- Dont have Word64; ADR
-- %fun SetFilePointer :: HANDLE -> Word64 -> FilePtrDirection -> IO Word64

----------------------------------------------------------------
-- File Notifications
--
-- Use these to initialise, "increment" and close a HANDLE you can wait
-- on.
----------------------------------------------------------------

findFirstChangeNotification :: String -> Bool -> FileNotificationFlag -> IO HANDLE
findFirstChangeNotification gc_arg1 gc_arg2 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_bool_ gc_arg2) >>= \ (arg2) ->
  prim_findFirstChangeNotification arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_findFirstChangeNotification_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_findFirstChangeNotification_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_findFirstChangeNotification_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_findFirstChangeNotification" prim_findFirstChangeNotification :: Addr -> Int -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_findFirstChangeNotification_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_findFirstChangeNotification_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_findFirstChangeNotification_gc_failstring :: Addr -> IO (Addr)

findNextChangeNotification :: HANDLE -> IO ()
findNextChangeNotification arg1 =
  prim_findNextChangeNotification arg1
  >>= \ gc_result ->
  access_prim_findNextChangeNotification_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_findNextChangeNotification_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_findNextChangeNotification" prim_findNextChangeNotification :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_findNextChangeNotification_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_findNextChangeNotification_gc_failstring :: Addr -> IO (Addr)

findCloseChangeNotification :: HANDLE -> IO ()
findCloseChangeNotification arg1 =
  prim_findCloseChangeNotification arg1
  >>= \ gc_result ->
  access_prim_findCloseChangeNotification_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_findCloseChangeNotification_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_findCloseChangeNotification" prim_findCloseChangeNotification :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_findCloseChangeNotification_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_findCloseChangeNotification_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- DOS Device flags
----------------------------------------------------------------

defineDosDevice :: DefineDosDeviceFlags -> String -> String -> IO ()
defineDosDevice arg1 gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  (marshall_string_ gc_arg2) >>= \ (arg3) ->
  prim_defineDosDevice arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_defineDosDevice_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_defineDosDevice_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_defineDosDevice" prim_defineDosDevice :: Word32 -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_defineDosDevice_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_defineDosDevice_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------

-- These functions are very unusual in the Win32 API:
-- They dont return error codes

areFileApisANSI :: IO Bool
areFileApisANSI =
  prim_areFileApisANSI
  >>= \  res1  ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_areFileApisANSI" prim_areFileApisANSI :: IO (Int)
setFileApisToOEM :: IO ()
setFileApisToOEM =
  prim_setFileApisToOEM
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_setFileApisToOEM" prim_setFileApisToOEM :: IO ()
setFileApisToANSI :: IO ()
setFileApisToANSI =
  prim_setFileApisToANSI
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_setFileApisToANSI" prim_setFileApisToANSI :: IO ()
setHandleCount :: UINT -> IO UINT
setHandleCount arg1 =
  prim_setHandleCount arg1
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_setHandleCount" prim_setHandleCount :: Word32 -> IO (Word32)

----------------------------------------------------------------

getLogicalDrives :: IO DWORD
getLogicalDrives =
  prim_getLogicalDrives
  >>= \ gc_result ->
  access_prim_getLogicalDrives_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getLogicalDrives_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getLogicalDrives_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_getLogicalDrives" prim_getLogicalDrives :: IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getLogicalDrives_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getLogicalDrives_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getLogicalDrives_gc_failstring :: Addr -> IO (Addr)

-- %fun GetDriveType :: MbString -> IO DriveType

getDiskFreeSpace :: MbString -> IO (DWORD,DWORD,DWORD,DWORD)
getDiskFreeSpace gc_arg1 =
  (case gc_arg1 of {
      Nothing -> (return (nullAddr));
      (Just gc_arg1) -> (marshall_string_ gc_arg1) >>= \ (s) ->
			(return ((s)))
   }) >>= \ (s) ->
  prim_getDiskFreeSpace s
  >>= \ gc_result ->
  access_prim_getDiskFreeSpace_res1 (gc_result :: Addr) >>= \ res1 ->
  access_prim_getDiskFreeSpace_res2 (gc_result :: Addr) >>= \ res2 ->
  access_prim_getDiskFreeSpace_res3 (gc_result :: Addr) >>= \ res3 ->
  access_prim_getDiskFreeSpace_res4 (gc_result :: Addr) >>= \ res4 ->
  access_prim_getDiskFreeSpace_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_getDiskFreeSpace_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return ((res1,res2,res3,res4)))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_getDiskFreeSpace" prim_getDiskFreeSpace :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getDiskFreeSpace_res1 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getDiskFreeSpace_res2 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getDiskFreeSpace_res3 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getDiskFreeSpace_res4 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getDiskFreeSpace_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_getDiskFreeSpace_gc_failstring :: Addr -> IO (Addr)

setVolumeLabel :: String -> String -> IO ()
setVolumeLabel gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  (marshall_string_ gc_arg2) >>= \ (arg2) ->
  prim_setVolumeLabel arg1 arg2
  >>= \ gc_result ->
  access_prim_setVolumeLabel_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_setVolumeLabel_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32File_stub_ffi.h prim_setVolumeLabel" prim_setVolumeLabel :: Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setVolumeLabel_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32File_stub_ffi.h" access_prim_setVolumeLabel_gc_failstring :: Addr -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
