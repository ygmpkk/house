module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants

import Control.Monad

main = do j_setdebug 2
	  rv <- j_start
	  when (j_FALSE == rv) (error "Could not start the JAPI server (jre or java)")
	  frame <- j_frame "Alert dialog demo"
	  j_show (Object $ unFrame frame)
          alert <- j_messagebox frame "www.iarchitect.com"
                                      "All examples are real Microsoft error warnings\n found at: www.iarchitect.com"
          j_sleep 5000
	  j_dispose alert
	  alert <- j_alertbox frame "Error Deleting File"
    	                            "Cannot delete 016: There is not enough free disk space\nDelete one ore more files to free disk space, and then try again."
    				    "OK"
	  putStrLn ("Alert = " ++ (show alert)) 
	  alert <- j_choicebox2 frame "Performance Warning"
		                      "A new MS-DOS resident program named 'WIN' may decrease your system's performance.\nWould you like to see more information about this problem?"
    		                      " Yes "
                                      "  No  " 
	  putStrLn ("Alert = " ++ (show alert)) 
	  alert <- j_choicebox3 frame "SQL Windows" "This item doesn't belong here"
    		                      "   Yes   " "    No    " "Cancel" 
	  putStrLn ("Alert = " ++ (show alert)) 
	  waitForFrameAction frame
	  return j_quit

waitForFrameAction :: Frame -> IO ()
waitForFrameAction frame = do rv <- j_nextaction
			      when (rv /= EventListener (unFrame frame)) (waitForFrameAction frame)
			      return ()
