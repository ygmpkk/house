{-# OPTIONS -optc-DSTANDALONE #-}
{-# OPTIONS -#include "config.h" #-}
{-# LINE 1 "Unicode.hsc" #-}
{-# OPTIONS -fno-implicit-prelude #-}
{-# LINE 2 "Unicode.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Unicde
-- Copyright   :  (c) The University of Glasgow, 2003
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Implementations for the character predicates (isLower, isUpper, etc.)
-- and the conversions (toUpper, toLower).  The implementation uses
-- libunicode on Unix systems if that is available.
--
-----------------------------------------------------------------------------

module GHC.Unicode (
    isAscii, isLatin1, isControl,
    isAsciiUpper, isAsciiLower,
    isPrint, isSpace,  isUpper,
    isLower, isAlpha,  isDigit,
    isOctDigit, isHexDigit, isAlphaNum,
    toUpper, toLower,
  ) where

import GHC.Base
import GHC.Real  (fromIntegral)
import GHC.Int
import GHC.Word
import GHC.Num	 (fromInteger)


{-# LINE 34 "Unicode.hsc" #-}

-- | Selects the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.
isAscii                 :: Char -> Bool
isAscii c	 	=  c <  '\x80'

-- | Selects the first 256 characters of the Unicode character set,
-- corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1                :: Char -> Bool
isLatin1 c              =  c <= '\xff'

isAsciiUpper, isAsciiLower :: Char -> Bool
isAsciiLower c          =  c >= 'a' && c <= 'z'
isAsciiUpper c          =  c >= 'A' && c <= 'Z'

-- | Selects control characters, which are the non-printing characters of
-- the Latin-1 subset of Unicode.
isControl               :: Char -> Bool

-- | Selects printable Unicode characters
-- (letters, numbers, marks, punctuation, symbols and spaces).
isPrint                 :: Char -> Bool

-- | Selects white-space characters in the Latin-1 range.
-- (In Unicode terms, this includes spaces and some control characters.)
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- Done with explicit equalities both for efficiency, and to avoid a tiresome
-- recursion with GHC.List elem
isSpace c		=  c == ' '	||
			   c == '\t'	||
			   c == '\n'	||
			   c == '\r'	||
			   c == '\f'	||
			   c == '\v'	||
			   c == '\xa0'

-- | Selects alphabetic Unicode characters (letters) that are not lower-case.
-- (In Unicode terms, this includes letters in upper and title cases,
-- as well as modifier letters and other letters.)
isUpper                 :: Char -> Bool

-- | Selects lower-case alphabetic Unicode characters (letters).
isLower                 :: Char -> Bool

-- | Selects alphabetic Unicode characters (letters).
isAlpha                 :: Char -> Bool

-- | Selects alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are selected by this
-- function but not by 'isDigit'.  Such digits may be part of identifiers
-- but are not used by the printer and reader to represent numbers.
isAlphaNum              :: Char -> Bool

-- | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit                 :: Char -> Bool

-- | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
isOctDigit              :: Char -> Bool
isOctDigit c		=  c >= '0' && c <= '7'

-- | Selects ASCII hexadecimal digits,
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
isHexDigit              :: Char -> Bool
isHexDigit c		=  isDigit c || c >= 'A' && c <= 'F' ||
                                        c >= 'a' && c <= 'f'

-- | Convert a letter to the corresponding upper-case letter, leaving any
-- other character unchanged.  Any Unicode letter which has an upper-case
-- equivalent is transformed.
toUpper                 :: Char -> Char

-- | Convert a letter to the corresponding lower-case letter, leaving any
-- other character unchanged.  Any Unicode letter which has a lower-case
-- equivalent is transformed.
toLower                 :: Char -> Char

-- -----------------------------------------------------------------------------
-- Win32 implementation


{-# LINE 175 "Unicode.hsc" #-}

isControl c		=  c < ' ' || c >= '\DEL' && c <= '\x9f'
isPrint c		=  not (isControl c)

-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
isUpper c		=  c >= 'A' && c <= 'Z' || 
                           c >= '\xC0' && c <= '\xD6' ||
                           c >= '\xD8' && c <= '\xDE'
-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.
isLower c		=  c >= 'a' && c <= 'z' ||
                           c >= '\xDF' && c <= '\xF6' ||
                           c >= '\xF8' && c <= '\xFF'

isAlpha c		=  isLower c || isUpper c
isDigit c		=  c >= '0' && c <= '9'
isAlphaNum c		=  isAlpha c || isDigit c

-- Case-changing operations

toUpper c@(C# c#)
  | isAsciiLower c    = C# (chr# (ord# c# -# 32#))
  | isAscii c         = c
    -- fall-through to the slower stuff.
  | isLower c	&& c /= '\xDF' && c /= '\xFF'
  = unsafeChr (ord c `minusInt` ord 'a' `plusInt` ord 'A')
  | otherwise
  = c


toLower c@(C# c#)
  | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
  | isAscii c      = c
  | isUpper c	   = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
  | otherwise	   =  c


{-# LINE 213 "Unicode.hsc" #-}

