module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants
import Control.Monad
-- import Japi
-- import Control.Monad

main = do j_setdebug 4
	  rv <- j_start
	  when (0 == rv) (error "Could not start the JAPI server (jre or java)")
	  frame <- j_frame "Frame demo"
	  j_show (Object $ (unFrame frame))
	  icon  <- j_loadimage "../c-examples/images/new.gif"
	  when (0 == icon) (error "Could not find the icon file.")
	  j_seticon frame icon
	  waitForFrameAction frame
	  return j_quit

waitForFrameAction :: Frame -> IO ()
waitForFrameAction frame = do rv <- j_nextaction
			      when (not (rv == (EventListener $ (unFrame frame)))) (waitForFrameAction frame)
			      return ()