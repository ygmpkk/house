module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants
import Control.Monad

main = do j_setdebug 2
	  rv <- j_start
	  when (j_FALSE == rv) (error "Could not start the JAPI server (jre or java)")
	  frame <- j_frame "Menus"
	  menubar <- j_menubar frame 
	  file <- j_menu menubar "File"
	  calc <- j_menu menubar "Calc"
	  quitMI <- j_menuitem file "Quit"
	  startMI <- j_menuitem calc "Start"
	  stopMI  <- j_menuitem calc "Stop"
	  canvas <- j_canvas frame 256 256
	  j_setpos canvas 10 60
	  j_setnamedcolorbg (Object (unCanvas canvas)) j_YELLOW
	  j_pack frame
	  j_show (Object (unFrame frame))
	  waitForFrameAction frame canvas False startMI stopMI quitMI
	  return j_quit

waitForFrameAction :: Frame -> Canvas -> Bool -> MenuItem -> MenuItem -> MenuItem -> IO ()
waitForFrameAction frame canvas dowork startMI stopMI quitMI = 
				  do act <- if dowork 
						then j_getaction
						else j_nextaction
				     when ((MenuItem (unEventListener act)) == quitMI) (return ())
				     (cont, wk) <- if (MenuItem (unEventListener act)) == startMI
							then do j_setnamedcolorbg (Object (unCanvas canvas)) j_YELLOW
					     			return (True, True)
					                else if (MenuItem (unEventListener act)) == stopMI
								then return (True, False)
								else return (True, dowork)
				     if cont
					then if wk
					       then do r1 <- j_random
						       r2 <- j_random
						       r3 <- j_random
						       r4 <- j_random
						       r5 <- j_random
						       j_setcolor (Object (unCanvas canvas))
								  (r1 `mod` 256) (r2 `mod` 256) (r3 `mod` 256)
					               j_drawpixel canvas (r4 `mod` 256) (r5 `mod` 256)
					               waitForFrameAction frame canvas wk startMI stopMI quitMI
					       else do waitForFrameAction frame canvas wk startMI stopMI quitMI
				        else return ()
