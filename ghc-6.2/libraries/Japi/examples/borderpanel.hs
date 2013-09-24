module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants
-- import Japi
import Control.Monad

main = do j_setdebug 4
	  rv <- j_start
	  when (j_FALSE == rv) (error "Could not start the JAPI server (jre or java)")
	  frame <- j_frame "j_borderpanel"
	  j_setcolorbg (Object $ unFrame frame) 220 220 220
	  j_setgridlayout frame 1 4
	  j_sethgap frame 10 

	  panel <- j_borderpanel frame j_LINEDOWN
	  j_setflowlayout panel j_HORIZONTAL
	  label <- j_label panel "LINEDOWN"
 
	  panel <- j_borderpanel frame j_LINEUP
	  j_setflowlayout panel j_HORIZONTAL
	  label <- j_label panel "LINEUP"

	  panel <- j_borderpanel frame j_AREADOWN
	  j_setflowlayout panel j_HORIZONTAL
	  label <- j_label panel "AREADOWN"

	  panel <- j_borderpanel frame j_AREAUP
	  j_setflowlayout panel j_HORIZONTAL
	  label <- j_label panel "AREAUP"

	  j_pack frame
	  j_show (Object $ unFrame frame)
	  waitForFrameAction frame
	  return j_quit

waitForFrameAction :: Frame -> IO ()
waitForFrameAction frame = do rv <- j_nextaction
			      when (rv /= EventListener (unFrame frame)) (waitForFrameAction frame)
			      return ()