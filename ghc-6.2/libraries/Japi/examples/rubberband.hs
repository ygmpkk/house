module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants
import Control.Monad
-- import Japi
-- import Control.Monad

main = do -- j_setdebug 3
	  rv <- j_start
	  when (j_FALSE == rv) (error "Could not start the JAPI server (jre or java)")
	  frame <- j_frame "Move and drag the mouse"
	  canvas <- j_canvas frame 230 220
	  j_setpos canvas 10 30
	  j_show (Object (unFrame frame))
	  j_pack frame
	  pressed <- j_mouselistener canvas j_PRESSED
	  dragged <- j_mouselistener canvas j_DRAGGED
	  j_setnamedcolor (Object $ unCanvas canvas) j_BLUE
	  j_fillrect canvas 0 0 230 110 
	  j_setnamedcolor (Object $ unCanvas canvas) j_YELLOW
	  j_fillrect canvas 0 110 230 110 
	  j_setnamedcolor (Object $ unCanvas canvas) j_RED
	  eventLoop 0 0 0 0 frame canvas pressed dragged
	  return j_quit

eventLoop :: Int -> Int -> Int -> Int -> Frame -> Canvas -> EventListener -> EventListener -> IO ()
eventLoop startx starty x y frame canvas pressed dragged =
    do obj <- j_nextaction
       (nstartx, nstarty, nx, ny) <- if obj == pressed then
				         do j_setxor canvas j_TRUE
					    j_drawrect canvas startx starty (x-startx) (y-starty)
				            (x, y ) <- j_getmousepos pressed
					    j_setxor canvas j_FALSE
					    return (x, y, x, y)
				     else 
				        if obj == dragged then
				           do j_setxor canvas j_TRUE
					      j_drawrect canvas startx starty (x-startx) (y-starty)
				              (x, y) <- j_getmousepos dragged 
					      j_drawrect canvas startx starty (x-startx) (y-starty)
					      j_setxor canvas j_FALSE
					      return (startx, starty, x, y)
				         else
				            return (startx, starty, x, y)
       when (obj /= EventListener (unFrame frame)) (eventLoop nstartx nstarty nx ny frame canvas pressed dragged)
       return ()

