{-# OPTIONS -optc-DSTANDALONE #-}
{-# OPTIONS -#include <sys/types.h> #-}
{-# OPTIONS -#include "regex.h" #-}
{-# LINE 1 "Posix.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "Posix.hsc" #-}
-- |
-- Module      :  Text.Regex.Posix
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (needs POSIX regexps)
--
-- Interface to the POSIX regular expression library.
--
-----------------------------------------------------------------------------

-- ToDo: should have an interface using PackedStrings.

module Text.Regex.Posix (
	-- * The @Regex@ type
	Regex,	 	-- abstract

	-- * Compiling a regular expression
	regcomp, 	-- :: String -> Int -> IO Regex

	-- ** Flags for regcomp
	regExtended,	-- (flag to regcomp) use extended regex syntax
	regIgnoreCase,	-- (flag to regcomp) ignore case when matching
	regNewline,	-- (flag to regcomp) '.' doesn't match newline

	-- * Matching a regular expression
	regexec, 	-- :: Regex		     -- pattern
	         	-- -> String		     -- string to match
	         	-- -> IO (Maybe (String,     -- everything before match
	         	-- 	 	 String,     -- matched portion
	         	--		 String,     -- everything after match
	         	-- 	 	 [String]))  -- subexpression matches

  ) where


{-# LINE 40 "Posix.hsc" #-}

{-# LINE 41 "Posix.hsc" #-}

import Prelude

import Foreign
import Foreign.C

-- | A compiled regular expression
newtype Regex = Regex (ForeignPtr CRegex)

-- -----------------------------------------------------------------------------
-- regcomp

-- | Compiles a regular expression
regcomp
  :: String  	-- ^ The regular expression to compile
  -> Int    	-- ^ Flags (summed together)
  -> IO Regex  	-- ^ Returns: the compiled regular expression
regcomp pattern flags = do
  regex_fptr <- mallocForeignPtrBytes (16)
{-# LINE 60 "Posix.hsc" #-}
  r <- withCString pattern $ \cstr ->
    	 withForeignPtr regex_fptr $ \p ->
           c_regcomp p cstr (fromIntegral flags)
  if (r == 0)
     then do addForeignPtrFinalizer ptr_regfree regex_fptr
	     return (Regex regex_fptr)
     else error "Text.Regex.Posix.regcomp: error in pattern" -- ToDo

-- -----------------------------------------------------------------------------
-- regexec

-- | Matches a regular expression against a string
regexec :: Regex			-- ^ Compiled regular expression
	-> String			-- ^ String to match against
	-> IO (Maybe (String, String, String, [String]))
	 	-- ^ Returns: 'Nothing' if the regex did not match the
		-- string, or:
		--
		-- @
		--   'Just' (everything before match,
		--         matched portion,
		--         everything after match,
		--         subexpression matches)
		-- @

regexec (Regex regex_fptr) str = do
  withCString str $ \cstr -> do
    withForeignPtr regex_fptr $ \regex_ptr -> do
      nsub <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) regex_ptr
{-# LINE 89 "Posix.hsc" #-}
      let nsub_int = fromIntegral (nsub :: CSize)
      allocaBytes ((1 + nsub_int) * (8)) $ \p_match -> do
{-# LINE 91 "Posix.hsc" #-}
		-- add one because index zero covers the whole match
        r <- c_regexec regex_ptr cstr (1 + nsub) p_match 0{-no flags for now-}

        if (r /= 0) then return Nothing else do 

        (before,match,after) <- matched_parts str p_match

        sub_strs <- 
	  mapM (unpack str) $ take nsub_int $ tail $
	     iterate (`plusPtr` (8)) p_match
{-# LINE 101 "Posix.hsc" #-}

        return (Just (before, match, after, sub_strs))

matched_parts :: String -> Ptr CRegMatch -> IO (String, String, String)
matched_parts string p_match = do
  start <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_match :: IO (Int32)
{-# LINE 107 "Posix.hsc" #-}
  end   <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p_match :: IO (Int32)
{-# LINE 108 "Posix.hsc" #-}
  let s = fromIntegral start; e = fromIntegral end
  return ( take s string, 
	   take (e-s) (drop s string),
	   drop e string )  

unpack :: String -> Ptr CRegMatch -> IO (String)
unpack string p_match = do
  start <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_match :: IO (Int32)
{-# LINE 116 "Posix.hsc" #-}
  end   <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p_match :: IO (Int32)
{-# LINE 117 "Posix.hsc" #-}
  -- the subexpression may not have matched at all, perhaps because it
  -- was optional.  In this case, the offsets are set to -1.
  if (start == -1) then return "" else do
  return (take (fromIntegral (end-start)) (drop (fromIntegral start) string))

-- -----------------------------------------------------------------------------
-- The POSIX regex C interface

-- Flags for regexec
regNotbol :: Int
regNotbol =  1
regNoteol :: Int
regNoteol =  2

{-# LINE 129 "Posix.hsc" #-}

-- Return values from regexec
regNomatch :: Int
regNomatch =  1

{-# LINE 133 "Posix.hsc" #-}
--	REG_ESPACE

-- Flags for regcomp
regExtended :: Int
regExtended =  1
regIgnoreCase  :: Int
regIgnoreCase  =  2
regNosub :: Int
regNosub =  4
regNewline :: Int
regNewline =  8

{-# LINE 141 "Posix.hsc" #-}

-- Error codes from regcomp
regBadbr :: Int
regBadbr =  10
regBadpat :: Int
regBadpat =  2
regBadrpt :: Int
regBadrpt =  13
regEcollate :: Int
regEcollate =  3
regEctype :: Int
regEctype =  4
regEescape :: Int
regEescape =  5
regEsubreg :: Int
regEsubreg =  6
regEbrack :: Int
regEbrack =  7
regEparen :: Int
regEparen =  8
regEbrace :: Int
regEbrace =  9
regErange :: Int
regErange =  11
regEspace :: Int
regEspace =  12

{-# LINE 156 "Posix.hsc" #-}

type CRegex    = ()
type CRegMatch = ()

foreign import ccall unsafe "regcomp"
  c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt

foreign import ccall  unsafe "&regfree"
  ptr_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regexec"
  c_regexec :: Ptr CRegex -> CString -> CSize
	    -> Ptr CRegMatch -> CInt -> IO CInt
