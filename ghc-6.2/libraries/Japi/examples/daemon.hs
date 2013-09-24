module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants
-- import Japi
import Control.Monad

main = do j_setdebug 4
	  rv <- j_start
	  when (0 == rv) (error "Could not start the JAPI server (jre or java)")
	  return j_quit
