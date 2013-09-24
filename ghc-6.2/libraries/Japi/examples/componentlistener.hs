module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants

import Control.Monad

main = do j_setdebug 3
	  rv <- j_start
	  when (j_FALSE == rv) (error "Could not start the JAPI server (jre or java)")
	  frame <- j_frame "Component Listener"
	  j_setgridlayout frame 1 1
	  text <- j_textarea (Object $ unFrame frame) 80 25
	  resized <- j_componentlistener (Object $ unTextArea text) (EventKind $ unMovedKind j_RESIZED)
	  moved   <- j_componentlistener (Object $ unFrame frame) (EventKind $ unEventKind j_MOVED)
	  hidden  <- j_componentlistener (Object $ unTextArea text) (EventKind $ unMovedKind $ j_HIDDEN)
	  shown   <- j_componentlistener (Object $ unTextArea text) (EventKind $ unMovedKind $ j_SHOWN)
	  j_show (Object $ unFrame frame)
	  j_pack frame
	  waitForFrameAction frame text resized moved hidden shown
	  return j_quit

waitForFrameAction :: Frame -> TextArea -> ComponentListener -> ComponentListener -> ComponentListener -> ComponentListener -> IO ()
waitForFrameAction frame text resized moved hidden shown = 
				  do obj <- j_nextaction
				     again <- if (Frame $ unEventListener obj) == frame 
						then (return False)
						else do when ((ComponentListener $ unEventListener obj) == resized)
							     (do r <- j_getrows (Object $ unTextArea text)
								 c <- j_getcolumns (Object $ unTextArea text)
								 j_appendtext (Object $ unTextArea text) ("resized to " 
										    ++ (show r) 
										    ++ " rows "
										    ++ (show c)
										    ++ " columns.\n"))
				     			when ((ComponentListener $ unEventListener obj) == moved)
							     (do j_appendtext (Object $ unTextArea text) "Frame moved.\n"
								 vis <- j_isvisible (Object $ unTextArea text)
								 if j_TRUE == vis 
									then do j_settext (Object $ unFrame frame)
											   "Move again to see the text!"
										j_hide (Object $ unTextArea text)
									else do j_settext (Object $ unFrame frame) "ComponentListener"
										j_show (Object $ unTextArea text))
				     			when ((ComponentListener $ unEventListener obj) == hidden)
							     (do j_appendtext (Object $ unTextArea text) "Text hidden\n")
				     			when ((ComponentListener $ unEventListener obj) == shown)
							     (do j_appendtext (Object $ unTextArea text) "Text shown\n")
							(return True)
				     if again 
					then waitForFrameAction frame text resized moved hidden shown
					else return ()