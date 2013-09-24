module Win32Process where

import Win32Types
import StdDIS

-- constant to wait for a very long time.
foreign import  ccall unsafe "Win32Process_stub_ffi.h prim_iNFINITE" iNFINITE :: DWORD

sleep :: DWORD -> IO ()
sleep arg1 =
  prim_sleep arg1
foreign import  ccall unsafe "Win32Process_stub_ffi.h prim_sleep" prim_sleep :: Word32 -> IO ()
