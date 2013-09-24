--
-- Spawning a Win32 subprocess
--

module Win32Spawn(
	spawn -- :: String -> IO (Handle, Handle, Handle)
	-- Spawn a Win32 sub-process, to run the command given
	-- by the String, returning handles for the child's
	--	stdin, stdout, stderr
	-- respectively
  ) where

import CTypes
import Ptr
import CString
import GHC.Handle
import System.Posix.Internals( FDType( RegularFile ) )
import Storable
import MarshalUtils
import Monad ( when )
import IO

foreign import "spawnProc" spawnProc :: CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

spawn :: String
	-> IO ( Handle  -- write handle to child's stdin
	      , Handle	-- read handle to child's stdout
	      , Handle  -- read handle to child's stderr
	      )
spawn cmd = 
  withCString cmd $ \ p_cmd ->
   withObject 0 $ \ p_wIn  ->
    withObject 0 $ \ p_rOut ->
     withObject 0 $ \ p_rErr -> do
       rc  <- spawnProc p_cmd p_wIn p_rOut p_rErr
       when (rc /= 0) (ioError (userError ("runProc: unable to spawn " ++ show cmd)))
       wIn <- peek p_wIn
       hIn <- openFd (fromIntegral wIn) (Just RegularFile) 
		     ("<fd " ++ show wIn ++ ">") WriteMode False False
       hSetBuffering hIn NoBuffering
       rOut <- peek p_rOut
       hOut <- openFd (fromIntegral rOut) (Just RegularFile) 
		      ("<fd " ++ show rOut ++ ">") ReadMode True False
       hSetBuffering hOut NoBuffering
       rErr <- peek p_rErr
       hErr <- openFd (fromIntegral rErr) (Just RegularFile) 
		      ("<fd " ++ show rErr ++ ">") ReadMode True False
       hSetBuffering hErr NoBuffering
       return (hIn, hOut, hErr)

