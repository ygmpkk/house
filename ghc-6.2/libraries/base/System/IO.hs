{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The standard IO library.
--
-----------------------------------------------------------------------------

module System.IO (
    -- * The IO monad

    IO,			       -- instance MonadFix
    fixIO,		       -- :: (a -> IO a) -> IO a

#ifndef STANDALONE

    -- * Files and handles

    FilePath,		       -- :: String

    Handle,		-- abstract, instance of: Eq, Show.

    -- ** Standard handles

    -- | Three handles are allocated during program initialisation,
    -- and are initially open.

    stdin, stdout, stderr,   -- :: Handle

    -- * Opening and closing files

    -- ** Opening files

    openFile,		       -- :: FilePath -> IOMode -> IO Handle
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

    -- ** Closing files

    hClose,		       -- :: Handle -> IO ()

    -- ** Special cases

    -- | These functions are also exported by the "Prelude".

    readFile,		       -- :: FilePath -> IO String
    writeFile,		       -- :: FilePath -> String -> IO ()
    appendFile,		       -- :: FilePath -> String -> IO ()

    -- ** File locking

    -- $locking

    -- * Operations on handles

    -- ** Determining the size of a file

    hFileSize,		       -- :: Handle -> IO Integer

    -- ** Detecting the end of input

    hIsEOF,		       -- :: Handle -> IO Bool
    isEOF,		       -- :: IO Bool

    -- ** Buffering operations

    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    hSetBuffering,	       -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,	       -- :: Handle -> IO BufferMode
    hFlush,		       -- :: Handle -> IO ()

    -- ** Repositioning handles

    hGetPosn,		       -- :: Handle -> IO HandlePosn
    hSetPosn,		       -- :: HandlePosn -> IO ()
    HandlePosn,                -- abstract, instance of: Eq, Show.

    hSeek,		       -- :: Handle -> SeekMode -> Integer -> IO ()
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
#if !defined(__NHC__)
    hTell,		       -- :: Handle -> IO Integer
#endif

    -- ** Handle properties

    hIsOpen, hIsClosed,        -- :: Handle -> IO Bool
    hIsReadable, hIsWritable,  -- :: Handle -> IO Bool
    hIsSeekable,               -- :: Handle -> IO Bool

    -- ** Terminal operations

#if !defined(__HUGS__) && !defined(__NHC__)
    hIsTerminalDevice,	 	-- :: Handle -> IO Bool

    hSetEcho,			-- :: Handle -> Bool -> IO ()
    hGetEcho,			-- :: Handle -> IO Bool
#endif

    -- ** Showing handle state

#ifdef __GLASGOW_HASKELL__
    hShow,			-- :: Handle -> IO String
#endif

    -- * Text input and output

    -- ** Text input

    hWaitForInput,	       -- :: Handle -> Int -> IO Bool
    hReady,		       -- :: Handle -> IO Bool
    hGetChar,		       -- :: Handle -> IO Char
    hGetLine,		       -- :: Handle -> IO [Char]
    hLookAhead,		       -- :: Handle -> IO Char
    hGetContents,	       -- :: Handle -> IO [Char]

    -- ** Text output

    hPutChar,		       -- :: Handle -> Char -> IO ()
    hPutStr,		       -- :: Handle -> [Char] -> IO ()
    hPutStrLn,		       -- :: Handle -> [Char] -> IO ()
    hPrint,		       -- :: Show a => Handle -> a -> IO ()

    -- ** Special cases for standard input and output

    -- | These functions are also exported by the "Prelude".

    interact,		       -- :: (String -> String) -> IO ()
    putChar,		       -- :: Char   -> IO ()
    putStr,		       -- :: String -> IO () 
    putStrLn,		       -- :: String -> IO ()
    print,		       -- :: Show a => a -> IO ()
    getChar,		       -- :: IO Char
    getLine,		       -- :: IO String
    getContents,	       -- :: IO String
    readIO,		       -- :: Read a => String -> IO a
    readLn,		       -- :: Read a => IO a

    -- * Binary input and output

#if !defined(__NHC__)
    openBinaryFile,	       -- :: FilePath -> IOMode -> IO Handle
#endif

#if !defined(__HUGS__) && !defined(__NHC__)
    hSetBinaryMode,	       -- :: Handle -> Bool -> IO ()
    hPutBuf,		       -- :: Handle -> Ptr a -> Int -> IO ()
    hGetBuf,		       -- :: Handle -> Ptr a -> Int -> IO Int
    hPutBufNonBlocking,	       -- :: Handle -> Ptr a -> Int -> IO Int
    hGetBufNonBlocking,	       -- :: Handle -> Ptr a -> Int -> IO Int
#endif

    module System.IO.Error,

#endif /* !STANDALONE */
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase	-- Together these four Prelude modules define
import GHC.Handle	-- all the stuff exported by IO for the GHC version
import GHC.IO
import GHC.ST		( fixST )
import GHC.Exception
import GHC.Num
import GHC.Read
import GHC.Show
#endif

#ifdef __HUGS__
import Hugs.IO
import Hugs.IOExts
#endif

#ifdef __NHC__
import IO
  ( Handle ()
  , HandlePosn ()
  , IOMode (ReadMode,WriteMode,AppendMode,ReadWriteMode)
  , BufferMode (NoBuffering,LineBuffering,BlockBuffering)
  , SeekMode (AbsoluteSeek,RelativeSeek,SeekFromEnd)
  , stdin, stdout, stderr
  , openFile                  -- :: FilePath -> IOMode -> IO Handle
  , hClose                    -- :: Handle -> IO ()
  , hFileSize                 -- :: Handle -> IO Integer
  , hIsEOF                    -- :: Handle -> IO Bool
  , isEOF                     -- :: IO Bool
  , hSetBuffering             -- :: Handle -> BufferMode -> IO ()
  , hGetBuffering             -- :: Handle -> IO BufferMode
  , hFlush                    -- :: Handle -> IO ()
  , hGetPosn                  -- :: Handle -> IO HandlePosn
  , hSetPosn                  -- :: HandlePosn -> IO ()
  , hSeek                     -- :: Handle -> SeekMode -> Integer -> IO ()
  , hWaitForInput             -- :: Handle -> Int -> IO Bool
  , hGetChar                  -- :: Handle -> IO Char
  , hGetLine                  -- :: Handle -> IO [Char]
  , hLookAhead                -- :: Handle -> IO Char
  , hGetContents              -- :: Handle -> IO [Char]
  , hPutChar                  -- :: Handle -> Char -> IO ()
  , hPutStr                   -- :: Handle -> [Char] -> IO ()
  , hIsOpen, hIsClosed        -- :: Handle -> IO Bool
  , hIsReadable, hIsWritable  -- :: Handle -> IO Bool
  , hIsSeekable               -- :: Handle -> IO Bool

  , IO ()
  , FilePath                  -- :: String
  )
import NHC.IOExtras (fixIO)
#endif

#ifndef STANDALONE

import System.IO.Error (
    isAlreadyExistsError, isDoesNotExistError,  -- :: IOError -> Bool
    isAlreadyInUseError, isFullError, 
    isEOFError, isIllegalOperation, 
    isPermissionError, isUserError, 
 
    ioeGetErrorString,         -- :: IOError -> String
    ioeGetHandle,              -- :: IOError -> Maybe Handle
    ioeGetFileName,            -- :: IOError -> Maybe FilePath
 
    try,                       -- :: IO a -> IO (Either IOError a)
 
    -- re-exports of Prelude names
    IOError,
    ioError,                   -- :: IOError -> IO a
    userError,                 -- :: String  -> IOError
    catch                      -- :: IO a    -> (IOError -> IO a) -> IO a
  )

-- -----------------------------------------------------------------------------
-- Standard IO

#ifndef __HUGS__
-- | Write a character to the standard output device
-- (same as 'hPutChar' 'stdout').

putChar         :: Char -> IO ()
putChar c       =  hPutChar stdout c

-- | Write a string to the standard output device
-- (same as 'hPutStr' 'stdout').

putStr          :: String -> IO ()
putStr s        =  hPutStr stdout s

-- | The same as 'putStrLn', but adds a newline character.

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s
                      putChar '\n'

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])

print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)

-- | Read a character from the standard input device
-- (same as 'hGetChar' 'stdin').

getChar         :: IO Char
getChar         =  hGetChar stdin

-- | Read a line from the standard input device
-- (same as 'hGetLine' 'stdin').

getLine         :: IO String
getLine         =  hGetLine stdin

-- | The 'getContents' operation returns all user input as a single string,
-- which is read lazily as it is needed
-- (same as 'hGetContents' 'stdin').

getContents     :: IO String
getContents     =  hGetContents stdin

-- | The 'interact' function takes a function of type @String->String@
-- as its argument.  The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string is
-- output on the standard output device.

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)

-- | The 'readFile' function reads a file and
-- returns the contents of the file as a string.
-- The file is read lazily, on demand, as with 'getContents'.

readFile        :: FilePath -> IO String
readFile name	=  openFile name ReadMode >>= hGetContents

-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.

writeFile       :: FilePath -> String -> IO ()
writeFile name str = do
    hdl <- openFile name WriteMode
    hPutStr hdl str
    hClose hdl

-- | The computation 'appendFile' @file str@ function appends the string @str@,
-- to the file @file@.
--
-- Note that 'writeFile' and 'appendFile' write a literal string
-- to a file.  To write a value of any printable type, as with 'print',
-- use the 'show' function to convert the value to a string first.
--
-- > main = appendFile "squares" (show [(x,x*x) | x <- [0,0.1..2]])

appendFile      :: FilePath -> String -> IO ()
appendFile name str = do
    hdl <- openFile name AppendMode
    hPutStr hdl str
    hClose hdl

-- | The 'readLn' function combines 'getLine' and 'readIO'.

readLn          :: Read a => IO a
readLn          =  do l <- getLine
                      r <- readIO l
                      return r

-- | The 'readIO' function is similar to 'read' except that it signals
-- parse failure to the 'IO' monad instead of terminating the program.

readIO          :: Read a => String -> IO a
readIO s        =  case (do { (x,t) <- reads s ;
			      ("","") <- lex t ;
                              return x }) of
			[x]    -> return x
			[]     -> ioError (userError "Prelude.readIO: no parse")
			_      -> ioError (userError "Prelude.readIO: ambiguous parse")
#endif  /* __HUGS__ */

-- | Computation 'hReady' @hdl@ indicates whether at least one item is
-- available for input from handle @hdl@.
-- 
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.

hReady		:: Handle -> IO Bool
hReady h 	=  hWaitForInput h 0

-- | The same as 'hPutStr', but adds a newline character.

hPutStrLn	:: Handle -> String -> IO ()
hPutStrLn hndl str = do
 hPutStr  hndl str
 hPutChar hndl '\n'

-- | Computation 'hPrint' @hdl t@ writes the string representation of @t@
-- given by the 'shows' function to the file or channel managed by @hdl@
-- and appends a newline.
--
-- This operation may fail with:
--
--  * 'isFullError' if the device is full; or
--
--  * 'isPermissionError' if another system resource limit would be exceeded.

hPrint		:: Show a => Handle -> a -> IO ()
hPrint hdl 	=  hPutStrLn hdl . show

#endif /* !STANDALONE */

-- ---------------------------------------------------------------------------
-- fixIO

#ifdef __GLASGOW_HASKELL__
fixIO 		:: (a -> IO a) -> IO a
fixIO m         = stToIO (fixST (ioToST . m))
#endif

#ifndef STANDALONE

-- $locking
-- Implementations should enforce as far as possible, at least locally to the
-- Haskell process, multiple-reader single-writer locking on files.
-- That is, /there may either be many handles on the same file which manage
-- input, or just one handle on the file which manages output/.  If any
-- open or semi-closed handle is managing a file for output, no new
-- handle can be allocated for that file.  If any open or semi-closed
-- handle is managing a file for input, new handles can only be allocated
-- if they do not manage output.  Whether two files are the same is
-- implementation-dependent, but they should normally be the same if they
-- have the same absolute path name and neither has been renamed, for
-- example.
--
-- /Warning/: the 'readFile' operation holds a semi-closed handle on
-- the file until the entire contents of the file have been consumed.
-- It follows that an attempt to write to a file (using 'writeFile', for
-- example) that was earlier opened by 'readFile' will usually result in
-- failure with 'isAlreadyInUseError'.

#endif /* !STANDALONE */
