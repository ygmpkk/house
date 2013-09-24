
--Stubs for Win32 API for accessing the registry.
module Win32Registry 
		( module Win32Registry
		, MbString
		) where
{- What's really on offer:
	(
	  regCloseKey        -- :: HKEY -> IO ()
	, regConnectRegistry -- :: MbString -> HKEY -> IO HKEY
	, regCreateKey       -- :: HKEY -> String -> IO HKEY
	, regCreateKeyEx     -- :: HKEY -> String -> String 
	                     -- -> RegCreateOptions -> REGSAM
			     -- -> MbLPSECURITY_ATTRIBUTES 
			     -- -> IO (HKEY, Bool)
        , regDeleteKey       -- :: HKEY -> String -> IO ()
	, regDeleteValue     -- :: HKEY -> String -> IO ()
	, regEnumKeys	     -- :: HKEY -> IO [String]
	, regEnumKey 	     -- :: HKEY -> DWORD -> Addr -> DWORD -> IO String
	, regEnumKeyValue    -- :: HKEY -> DWORD -> Addr -> DWORD -> Addr -> DWORD -> IO String
	, regFlushKey        -- :: HKEY -> IO ()
	, regLoadKey         -- :: HKEY -> String -> String -> IO ()
	, regNotifyChangeKeyValue -- :: HKEY -> Bool -> RegNotifyOptions 
				  -- -> HANDLE -> Bool -> IO ()
	, regOpenKey         -- :: HKEY -> String -> IO HKEY
	, regOpenKeyEx 	     -- :: HKEY -> String -> REGSAM -> IO HKEY
	, regQueryInfoKey    -- :: HKEY -> IO RegInfoKey
	, regQueryValue      -- :: HKEY -> MbString -> IO String
	, regQueryValueKey   -- :: HKEY -> MbString -> IO String
	, regQueryValueEx    -- :: HKEY -> String -> Addr -> Int -> IO RegValueType
	, regReplaceKey      -- :: HKEY -> String -> String -> String -> IO ()
	, regRestoreKey      -- :: HKEY -> String -> RegRestoreFlags -> IO ()
	, regSaveKey         -- :: HKEY -> String -> MbLPSECURITY_ATTRIBUTES -> IO ()
	, regSetValue        -- :: HKEY -> String -> String -> IO ()
	, regSetValueEx      -- :: HKEY -> String -> RegValueType -> Addr -> Int -> IO ()
	, regSetStringValue  -- :: HKEY -> String -> String -> IO ()
	, regUnloadKey       -- :: HKEY -> String -> IO ()
	) where
-}

{-
 Registry API omissions:

   RegQueryMultipleValues()
   RegEnumKeyEx()

-}

import StdDIS
import Addr
import Word
import Win32Types
import Win32File
import Foreign.ForeignPtr

hKEY_CLASSES_ROOT :: HKEY
hKEY_CLASSES_ROOT =
  unsafePerformIO(
    prim_hKEY_CLASSES_ROOT
    >>= \ gc_result ->
    access_prim_hKEY_CLASSES_ROOT_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
    access_prim_hKEY_CLASSES_ROOT_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
    (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
    (return (gc_res2)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_hKEY_CLASSES_ROOT" prim_hKEY_CLASSES_ROOT :: IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_CLASSES_ROOT_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_CLASSES_ROOT_gc_res1 :: Addr -> IO (Addr)
hKEY_CURRENT_CONFIG :: HKEY
hKEY_CURRENT_CONFIG =
  unsafePerformIO(
    prim_hKEY_CURRENT_CONFIG
    >>= \ gc_result ->
    access_prim_hKEY_CURRENT_CONFIG_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
    access_prim_hKEY_CURRENT_CONFIG_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
    (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
    (return (gc_res2)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_hKEY_CURRENT_CONFIG" prim_hKEY_CURRENT_CONFIG :: IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_CURRENT_CONFIG_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_CURRENT_CONFIG_gc_res1 :: Addr -> IO (Addr)
hKEY_CURRENT_USER :: HKEY
hKEY_CURRENT_USER =
  unsafePerformIO(
    prim_hKEY_CURRENT_USER
    >>= \ gc_result ->
    access_prim_hKEY_CURRENT_USER_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
    access_prim_hKEY_CURRENT_USER_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
    (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
    (return (gc_res2)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_hKEY_CURRENT_USER" prim_hKEY_CURRENT_USER :: IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_CURRENT_USER_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_CURRENT_USER_gc_res1 :: Addr -> IO (Addr)
hKEY_LOCAL_MACHINE :: HKEY
hKEY_LOCAL_MACHINE =
  unsafePerformIO(
    prim_hKEY_LOCAL_MACHINE
    >>= \ gc_result ->
    access_prim_hKEY_LOCAL_MACHINE_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
    access_prim_hKEY_LOCAL_MACHINE_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
    (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
    (return (gc_res2)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_hKEY_LOCAL_MACHINE" prim_hKEY_LOCAL_MACHINE :: IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_LOCAL_MACHINE_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_LOCAL_MACHINE_gc_res1 :: Addr -> IO (Addr)
hKEY_USERS :: HKEY
hKEY_USERS =
  unsafePerformIO(
    prim_hKEY_USERS
    >>= \ gc_result ->
    access_prim_hKEY_USERS_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
    access_prim_hKEY_USERS_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
    (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
    (return (gc_res2)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_hKEY_USERS" prim_hKEY_USERS :: IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_USERS_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_hKEY_USERS_gc_res1 :: Addr -> IO (Addr)

regCloseKey :: HKEY -> IO ()
regCloseKey arg1 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_regCloseKey arg1
  >>= \ gc_result ->
  access_prim_regCloseKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regCloseKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regCloseKey" prim_regCloseKey :: Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCloseKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCloseKey_gc_failstring :: Addr -> IO (Addr)

-- Connects to a predefined registry handle on another computer.
regConnectRegistry :: MbString -> HKEY -> IO HKEY
regConnectRegistry gc_arg1 arg2 =
  (case gc_arg1 of {
      Nothing -> (return (nullAddr));
      (Just gc_arg1) -> (marshall_string_ gc_arg1) >>= \ (arg1) ->
			(return ((arg1)))
   }) >>= \ (arg1) ->
  withForeignPtr arg2 $ \ arg2 ->
  prim_regConnectRegistry arg1 arg2
  >>= \ gc_result ->
  access_prim_regConnectRegistry_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regConnectRegistry_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_regConnectRegistry_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regConnectRegistry_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regConnectRegistry" prim_regConnectRegistry :: Addr -> Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regConnectRegistry_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regConnectRegistry_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regConnectRegistry_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regConnectRegistry_gc_failstring :: Addr -> IO (Addr)

regCreateKey :: HKEY -> String -> IO HKEY
regCreateKey arg1 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regCreateKey arg1 arg2
  >>= \ gc_result ->
  access_prim_regCreateKey_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regCreateKey_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_regCreateKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regCreateKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regCreateKey" prim_regCreateKey :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKey_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKey_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKey_gc_failstring :: Addr -> IO (Addr)

type RegCreateOptions = Int32

foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_OPTION_NON_VOLATILE" rEG_OPTION_NON_VOLATILE :: RegCreateOptions
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_OPTION_VOLATILE" rEG_OPTION_VOLATILE :: RegCreateOptions

type REGSAM = Int32

foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_ALL_ACCESS" kEY_ALL_ACCESS :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_CREATE_LINK" kEY_CREATE_LINK :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_CREATE_SUB_KEY" kEY_CREATE_SUB_KEY :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_ENUMERATE_SUB_KEYS" kEY_ENUMERATE_SUB_KEYS :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_EXECUTE" kEY_EXECUTE :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_NOTIFY" kEY_NOTIFY :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_QUERY_VALUE" kEY_QUERY_VALUE :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_READ" kEY_READ :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_SET_VALUE" kEY_SET_VALUE :: REGSAM
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_kEY_WRITE" kEY_WRITE :: REGSAM


regCreateKeyEx :: HKEY -> String -> String -> RegCreateOptions -> REGSAM -> MbLPSECURITY_ATTRIBUTES -> IO (HKEY,Bool)
regCreateKeyEx arg1 gc_arg1 gc_arg2 arg4 arg5 arg6 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  (marshall_string_ gc_arg2) >>= \ (arg3) ->
  (case arg6 of {
      Nothing -> (return (nullAddr));
      (Just arg6) -> (return ((arg6)))
   }) >>= \ (arg6) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regCreateKeyEx arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_regCreateKeyEx_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regCreateKeyEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_regCreateKeyEx_gc_res5 (gc_result :: Addr) >>= \ gc_res5 ->
  access_prim_regCreateKeyEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regCreateKeyEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (unmarshall_bool_ gc_res5) >>= \ gc_res4 ->
       (return ((gc_res2,gc_res4)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regCreateKeyEx" prim_regCreateKeyEx :: Ptr () -> Addr -> Addr -> Int32 -> Int32 -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKeyEx_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKeyEx_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKeyEx_gc_res5 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKeyEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regCreateKeyEx_gc_failstring :: Addr -> IO (Addr)

regDeleteKey :: HKEY -> String -> IO ()
regDeleteKey arg1 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regDeleteKey arg1 arg2
  >>= \ gc_result ->
  access_prim_regDeleteKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regDeleteKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regDeleteKey" prim_regDeleteKey :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regDeleteKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regDeleteKey_gc_failstring :: Addr -> IO (Addr)

regDeleteValue :: HKEY -> String -> IO ()
regDeleteValue arg1 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regDeleteValue arg1 arg2
  >>= \ gc_result ->
  access_prim_regDeleteValue_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regDeleteValue_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regDeleteValue" prim_regDeleteValue :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regDeleteValue_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regDeleteValue_gc_failstring :: Addr -> IO (Addr)

regEnumKeys :: HKEY -> IO [String]
regEnumKeys hkey = do
   hinfo <- regQueryInfoKey hkey
   let buflen = 1+max_subkey_len hinfo
   buf   <- malloc buflen
   ls    <- go 0 buf buflen
   free buf
   return ls
 where
   go n buf buflen = do
      (v,flg)  <- regEnumKey hkey n buf buflen
      if flg /= 0
       then return []
       else do
         vs <- go (n+1) buf buflen
         return (v:vs)

regEnumKeyVals :: HKEY -> IO [(String,String,RegValueType)]
regEnumKeyVals hkey = do
   hinfo <- regQueryInfoKey hkey
   let nmlen  = 1+max_value_name_len hinfo  -- add spc for terminating NUL.
   let vallen = 1+max_value_len hinfo
   nmbuf  <- malloc nmlen
   valbuf <- malloc vallen
   ls     <- go 0 nmbuf nmlen valbuf vallen
   free nmbuf
   free valbuf
   return ls
 where
   go n nmbuf nmlen valbuf vallen = do
      (ty,nm,flg) <- regEnumValue hkey n nmbuf nmlen valbuf vallen
      if flg /= 0
       then return []
       else do

        val <- 
	   case ty of
	     x | x == rEG_SZ    -> unmarshall_string_ valbuf
	       | x == rEG_DWORD -> readWord32Addr valbuf 0 >>= \ v -> return (show v)
	       | otherwise      -> return "<<unknown>>"

        vs <- go (n+1) nmbuf nmlen valbuf vallen
        return ((nm,val,ty):vs)

readWord32Addr :: Addr -> Int -> IO DWORD
readWord32Addr s i =
  prim_readWord32Addr s i
  >>= \  res1  ->
  (return (res1))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_readWord32Addr" prim_readWord32Addr :: Addr -> Int -> IO (Word32)

-- It's up to the programmer to ensure that a large enough
-- buffer is passed in here.

regEnumKey :: HKEY -> DWORD -> Addr -> DWORD -> IO (String,Int)
regEnumKey arg1 arg2 arg3 arg4 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_regEnumKey arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_regEnumKey_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_regEnumKey_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regEnumKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regEnumKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res2) >>= \ gc_res1 ->
       (return ((gc_res1,gc_res3)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regEnumKey" prim_regEnumKey :: Ptr () -> Word32 -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumKey_gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumKey_gc_res3 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumKey_gc_failstring :: Addr -> IO (Addr)

regEnumValue :: HKEY -> DWORD -> Addr -> DWORD -> Addr -> DWORD -> IO (RegValueType,String,Int)
regEnumValue arg1 arg2 arg3 arg4 arg5 arg6 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_regEnumValue arg1 arg2 arg3 arg4 arg5 arg6
  >>= \ gc_result ->
  access_prim_regEnumValue_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_regEnumValue_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regEnumValue_gc_res4 (gc_result :: Addr) >>= \ gc_res4 ->
  access_prim_regEnumValue_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regEnumValue_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res3) >>= \ gc_res2 ->
       (return ((gc_res1,gc_res2,gc_res4)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regEnumValue" prim_regEnumValue :: Ptr () -> Word32 -> Addr -> Word32 -> Addr -> Word32 -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumValue_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumValue_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumValue_gc_res4 :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumValue_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regEnumValue_gc_failstring :: Addr -> IO (Addr)

regFlushKey :: HKEY -> IO ()
regFlushKey arg1 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_regFlushKey arg1
  >>= \ gc_result ->
  access_prim_regFlushKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regFlushKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regFlushKey" prim_regFlushKey :: Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regFlushKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regFlushKey_gc_failstring :: Addr -> IO (Addr)

-- #ifdef FOR_WINDOWS_NT
-- %fun RegGetKeySecurity :: HKEY -> SECURITY_INFORMATION -> IO SECURITY_DESCRIPTION

-- #endif


regLoadKey :: HKEY -> String -> String -> IO ()
regLoadKey arg1 gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  (marshall_string_ gc_arg2) >>= \ (arg3) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regLoadKey arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_regLoadKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regLoadKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regLoadKey" prim_regLoadKey :: Ptr () -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regLoadKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regLoadKey_gc_failstring :: Addr -> IO (Addr)

-- #ifdef FOR_WINDOWS_NT

type RegNotifyOptions = Int32

foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_NOTIFY_CHANGE_NAME" rEG_NOTIFY_CHANGE_NAME :: RegNotifyOptions
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_NOTIFY_CHANGE_ATTRIBUTES" rEG_NOTIFY_CHANGE_ATTRIBUTES :: RegNotifyOptions
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_NOTIFY_CHANGE_LAST_SET" rEG_NOTIFY_CHANGE_LAST_SET :: RegNotifyOptions
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_NOTIFY_CHANGE_SECURITY" rEG_NOTIFY_CHANGE_SECURITY :: RegNotifyOptions



regNotifyChangeKeyValue :: HKEY -> Bool -> RegNotifyOptions -> HANDLE -> Bool -> IO ()
regNotifyChangeKeyValue arg1 gc_arg1 arg3 arg4 gc_arg2 =
  (marshall_bool_ gc_arg1) >>= \ (arg2) ->
  (marshall_bool_ gc_arg2) >>= \ (arg5) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regNotifyChangeKeyValue arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_regNotifyChangeKeyValue_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regNotifyChangeKeyValue_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regNotifyChangeKeyValue" prim_regNotifyChangeKeyValue :: Ptr () -> Int -> Int32 -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regNotifyChangeKeyValue_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regNotifyChangeKeyValue_gc_failstring :: Addr -> IO (Addr)

-- #endif

-- for Win 3.x compatibility, use RegOpenKeyEx instead.
regOpenKey :: HKEY -> String -> IO HKEY
regOpenKey arg1 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regOpenKey arg1 arg2
  >>= \ gc_result ->
  access_prim_regOpenKey_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regOpenKey_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_regOpenKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regOpenKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regOpenKey" prim_regOpenKey :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKey_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKey_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKey_gc_failstring :: Addr -> IO (Addr)

regOpenKeyEx :: HKEY -> String -> REGSAM -> IO HKEY
regOpenKeyEx arg1 gc_arg1 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regOpenKeyEx arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_regOpenKeyEx_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regOpenKeyEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_regOpenKeyEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regOpenKeyEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (makeForeignPtr gc_res1 gc_res3) >>= \ gc_res2 ->
       (return (gc_res2))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regOpenKeyEx" prim_regOpenKeyEx :: Ptr () -> Addr -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKeyEx_gc_res3 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKeyEx_gc_res1 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKeyEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regOpenKeyEx_gc_failstring :: Addr -> IO (Addr)

data RegInfoKey =
 RegInfoKey {
    class_string       :: String,
    class_id           :: Int,
    subkeys            :: Word32,
    max_subkey_len     :: Word32,
    max_class_len      :: Word32,
    values             :: Word32,
    max_value_name_len :: Word32,
    max_value_len      :: Word32,
    sec_len            :: Int,
    lastWrite_lo       :: Word32,
    lastWrite_hi       :: Word32
 }

regQueryInfoKey :: HKEY -> IO RegInfoKey
regQueryInfoKey arg1 =
  withForeignPtr arg1 $ \ arg1 ->
  prim_regQueryInfoKey arg1
  >>= \ gc_result ->
  access_prim_regQueryInfoKey_str (gc_result :: Addr) >>= \ str ->
  access_prim_regQueryInfoKey_cbClass (gc_result :: Addr) >>= \ cbClass ->
  access_prim_regQueryInfoKey_cSubKeys (gc_result :: Addr) >>= \ cSubKeys ->
  access_prim_regQueryInfoKey_cbMaxSubKeyLen (gc_result :: Addr) >>= \ cbMaxSubKeyLen ->
  access_prim_regQueryInfoKey_cbMaxClassLen (gc_result :: Addr) >>= \ cbMaxClassLen ->
  access_prim_regQueryInfoKey_cValues (gc_result :: Addr) >>= \ cValues ->
  access_prim_regQueryInfoKey_cbMaxValueNameLen (gc_result :: Addr) >>= \ cbMaxValueNameLen ->
  access_prim_regQueryInfoKey_cbMaxValueLen (gc_result :: Addr) >>= \ cbMaxValueLen ->
  access_prim_regQueryInfoKey_cbSecurityDescriptor (gc_result :: Addr) >>= \ cbSecurityDescriptor ->
  access_prim_regQueryInfoKey_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_regQueryInfoKey_gc_res3 (gc_result :: Addr) >>= \ gc_res3 ->
  access_prim_regQueryInfoKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regQueryInfoKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ str) >>= \ gc_res1 ->
       (return ((RegInfoKey gc_res1 cbClass cSubKeys cbMaxSubKeyLen cbMaxClassLen cValues cbMaxValueNameLen cbMaxValueLen cbSecurityDescriptor gc_res2 gc_res3)))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regQueryInfoKey" prim_regQueryInfoKey :: Ptr () -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_str :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cbClass :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cSubKeys :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cbMaxSubKeyLen :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cbMaxClassLen :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cValues :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cbMaxValueNameLen :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cbMaxValueLen :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_cbSecurityDescriptor :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_gc_res2 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_gc_res3 :: Addr -> IO (Word32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryInfoKey_gc_failstring :: Addr -> IO (Addr)

-- %fun RegQueryMultipleValues :: HKEY -> IO ([VALENT],String)

-- RegQueryValue() isn't really that, it just allows you to
-- get at the default values of keys, so we provide our own
-- (and better!) version of it. If you want RegQueryValue()s
-- behaviour, use regQueryValueKey.

regQueryValueKey :: HKEY -> MbString -> IO String
regQueryValueKey arg1 gc_arg1 =
  (case gc_arg1 of {
      Nothing -> (return (nullAddr));
      (Just gc_arg1) -> (marshall_string_ gc_arg1) >>= \ (arg2) ->
			(return ((arg2)))
   }) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regQueryValueKey arg1 arg2
  >>= \ gc_result ->
  access_prim_regQueryValueKey_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_regQueryValueKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regQueryValueKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res2) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regQueryValueKey" prim_regQueryValueKey :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValueKey_gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValueKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValueKey_gc_failstring :: Addr -> IO (Addr)

regQueryValue :: HKEY -> MbString -> IO String
regQueryValue arg1 gc_arg1 =
  (case gc_arg1 of {
      Nothing -> (return (nullAddr));
      (Just gc_arg1) -> (marshall_string_ gc_arg1) >>= \ (arg2) ->
			(return ((arg2)))
   }) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regQueryValue arg1 arg2
  >>= \ gc_result ->
  access_prim_regQueryValue_gc_res2 (gc_result :: Addr) >>= \ gc_res2 ->
  access_prim_regQueryValue_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regQueryValue_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (unmarshall_string_ gc_res2) >>= \ gc_res1 ->
       (return (gc_res1))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regQueryValue" prim_regQueryValue :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValue_gc_res2 :: Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValue_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValue_gc_failstring :: Addr -> IO (Addr)

-- %end free(szValue);

regQueryValueEx :: HKEY -> String -> Addr -> Int -> IO RegValueType
regQueryValueEx arg1 gc_arg1 arg3 arg4 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regQueryValueEx arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_regQueryValueEx_gc_res1 (gc_result :: Addr) >>= \ gc_res1 ->
  access_prim_regQueryValueEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regQueryValueEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (gc_res1))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regQueryValueEx" prim_regQueryValueEx :: Ptr () -> Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValueEx_gc_res1 :: Addr -> IO (Int32)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValueEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regQueryValueEx_gc_failstring :: Addr -> IO (Addr)

regReplaceKey :: HKEY -> String -> String -> String -> IO ()
regReplaceKey arg1 gc_arg1 gc_arg2 gc_arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  (marshall_string_ gc_arg2) >>= \ (arg3) ->
  (marshall_string_ gc_arg3) >>= \ (arg4) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regReplaceKey arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_regReplaceKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regReplaceKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regReplaceKey" prim_regReplaceKey :: Ptr () -> Addr -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regReplaceKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regReplaceKey_gc_failstring :: Addr -> IO (Addr)

type RegRestoreFlags = Int32

foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_WHOLE_HIVE_VOLATILE" rEG_WHOLE_HIVE_VOLATILE :: RegRestoreFlags
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_REFRESH_HIVE" rEG_REFRESH_HIVE :: RegRestoreFlags
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_NO_LAZY_FLUSH" rEG_NO_LAZY_FLUSH :: RegRestoreFlags

regRestoreKey :: HKEY -> String -> RegRestoreFlags -> IO ()
regRestoreKey arg1 gc_arg1 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regRestoreKey arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_regRestoreKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regRestoreKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regRestoreKey" prim_regRestoreKey :: Ptr () -> Addr -> Int32 -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regRestoreKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regRestoreKey_gc_failstring :: Addr -> IO (Addr)

regSaveKey :: HKEY -> String -> MbLPSECURITY_ATTRIBUTES -> IO ()
regSaveKey arg1 gc_arg1 arg3 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  (case arg3 of {
      Nothing -> (return (nullAddr));
      (Just arg3) -> (return ((arg3)))
   }) >>= \ (arg3) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regSaveKey arg1 arg2 arg3
  >>= \ gc_result ->
  access_prim_regSaveKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regSaveKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regSaveKey" prim_regSaveKey :: Ptr () -> Addr -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regSaveKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regSaveKey_gc_failstring :: Addr -> IO (Addr)

-- #ifdef FOR_WINDOWS_NT

-- %fun RegSetKeySecurity :: HKEY -> SECURITY_INFORMATION -> SECURITY_DESCRIPTOR -> IO ()

-- #endif

-- 3.1 compat. - only allows storage of REG_SZ values.
regSetValue :: HKEY -> String -> String -> IO ()
regSetValue arg1 gc_arg1 gc_arg2 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  (marshall_stringLen_ gc_arg2) >>= \ (arg3,arg4) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regSetValue arg1 arg2 arg3 arg4
  >>= \ gc_result ->
  access_prim_regSetValue_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regSetValue_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regSetValue" prim_regSetValue :: Ptr () -> Addr -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regSetValue_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regSetValue_gc_failstring :: Addr -> IO (Addr)


type RegValueType = Int32

foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_BINARY" rEG_BINARY :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_DWORD" rEG_DWORD :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_DWORD_LITTLE_ENDIAN" rEG_DWORD_LITTLE_ENDIAN :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_DWORD_BIG_ENDIAN" rEG_DWORD_BIG_ENDIAN :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_LINK" rEG_LINK :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_MULTI_SZ" rEG_MULTI_SZ :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_NONE" rEG_NONE :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_RESOURCE_LIST" rEG_RESOURCE_LIST :: RegValueType
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_rEG_SZ" rEG_SZ :: RegValueType


-- regSetValueEx has a somewhat wieldly interface if all you want to do is
-- add a string value (a Common Thing to want to do), so we support this
-- specially:
regSetStringValue :: HKEY -> String -> String -> IO ()
regSetStringValue hk key val = do
   v <- marshall_string_ val
   regSetValueEx hk key rEG_SZ v (length val)
   free v
   return ()

regSetValueEx :: HKEY -> String -> RegValueType -> Addr -> Int -> IO ()
regSetValueEx arg1 gc_arg1 arg3 arg4 arg5 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regSetValueEx arg1 arg2 arg3 arg4 arg5
  >>= \ gc_result ->
  access_prim_regSetValueEx_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regSetValueEx_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regSetValueEx" prim_regSetValueEx :: Ptr () -> Addr -> Int32 -> Addr -> Int -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regSetValueEx_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regSetValueEx_gc_failstring :: Addr -> IO (Addr)

regUnLoadKey :: HKEY -> String -> IO ()
regUnLoadKey arg1 gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg2) ->
  withForeignPtr arg1 $ \ arg1 ->
  prim_regUnLoadKey arg1 arg2
  >>= \ gc_result ->
  access_prim_regUnLoadKey_gc_failed (gc_result :: Addr) >>= \ gc_failed ->
  access_prim_regUnLoadKey_gc_failstring (gc_result :: Addr) >>= \ gc_failstring ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (()))
foreign import  ccall unsafe "Win32Registry_stub_ffi.h prim_regUnLoadKey" prim_regUnLoadKey :: Ptr () -> Addr -> IO (Addr)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regUnLoadKey_gc_failed :: Addr -> IO (Int)
foreign import ccall unsafe "Win32Registry_stub_ffi.h" access_prim_regUnLoadKey_gc_failstring :: Addr -> IO (Addr)

