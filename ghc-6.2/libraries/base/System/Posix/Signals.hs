{-# OPTIONS -optc-DSTANDALONE #-}
{-# OPTIONS -#include "config.h" #-}
{-# OPTIONS -#include "Signals.h" #-}
{-# LINE 1 "Signals.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "Signals.hsc" #-}
-- |
-- Module      :  System.Posix.Signals
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX signal support
--
-----------------------------------------------------------------------------


{-# LINE 16 "Signals.hsc" #-}

module System.Posix.Signals (

{-# LINE 88 "Signals.hsc" #-}
  ) where


{-# LINE 91 "Signals.hsc" #-}

import Foreign
import Foreign.C
import System.IO.Unsafe
import System.Posix.Types
import System.Posix.Internals


{-# LINE 442 "Signals.hsc" #-}

