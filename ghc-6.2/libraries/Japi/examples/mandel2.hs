module Main(main) where

import Graphics.UI.Japi.Binding
import Graphics.UI.Japi.Types
import Graphics.UI.Japi.Constants
import Control.Monad

data CalcState = CalcState { x, y, width, height :: Int,
                             lxs, xstart, lys, ystart, lxe, xend, lye, yend :: Double,
                             dowork :: Bool,
                             print, quit, start, stop, reset :: MenuItem }

main = do -- j_setdebug 4
          rv <- j_start
          when (j_FALSE == rv) (error "Could not start the JAPI server (jre or java)")
          frame <- j_frame ""
          j_setborderlayout frame
          menubar <- j_menubar frame 
          file <- j_menu menubar "File"
          calc <- j_menu menubar "Calc"
          prin  <- j_menuitem file "Print"
          qui  <- j_menuitem file "Quit"
          star  <- j_menuitem calc "Start"
          sto   <- j_menuitem calc "Stop"
          rese   <- j_menuitem calc "Reset"
	  ics <- ( return (CalcState { width  =  640,
				       height =  480,
				       lxs    = -1.8,
				       xstart = -1.8,
				       lxe    =  0.8,
				       xend   =  0.8,
				       lys    = -1.0,
				       ystart = -1.0,
				       lye    =  1.0,
				       yend   =  1.0,
				       x      = -1,
				       y      = -1,
				       dowork = False,
				       Main.print  = prin ,
				       quit   = qui ,
				       start  = star ,
				       stop   = sto ,
				       reset  = rese }) )
          canvas <- j_canvas frame (width ics) (height ics)
          pressed <- j_mouselistener canvas j_PRESSED
	  dragged <- j_mouselistener canvas j_DRAGGED
          j_setpos canvas 10 60
          j_setnamedcolorbg (Object (unCanvas canvas)) j_YELLOW
          j_pack frame
          j_show (Object (unFrame frame))

          waitForFrameAction frame canvas ics

          return j_quit
        
elToMenuItem :: EventListener -> MenuItem
elToMenuItem obj = MenuItem (unEventListener obj)

elToCanvas :: EventListener -> Canvas
elToCanvas obj = Canvas (unEventListener obj)

startCalcState :: CalcState -> CalcState
startCalcState cs = cs { xstart = lxs cs,
			 xend   = lxe cs,
			 ystart = lys cs,
			 yend   = lye cs,
			 x      = -1,
			 y      = -1,
			 dowork = True }

resetCalcState :: CalcState -> CalcState
resetCalcState cs =  cs { lxs    = -1.8,
			  xstart = -1.8,
			  lxe    =  0.8,
			  xend   =  0.8,
			  lys    = -1.0,
			  ystart = -1.0,
			  lye    =  1.0,
			  yend   =  1.0,
			  x      = -1,
			  y      = -1 }

stopCalcState :: CalcState -> CalcState
stopCalcState cs = cs { dowork = False }

waitForFrameAction :: Frame -> Canvas -> CalcState -> IO CalcState
waitForFrameAction frame canvas calcState = 
    do obj <- if dowork calcState
	          then j_getaction
                  else j_nextaction
       objMI <- return $ elToMenuItem $ obj
       objCan <- return $ elToCanvas $ obj
       (again, state) <- if objMI == quit calcState then return (False, calcState) else
			 if objMI == start calcState then do j_setnamedcolorbg canvasobj j_WHITE
				    			     return (True, startCalcState calcState)
						     else
			 if objMI == reset calcState then do j_setnamedcolorbg canvasobj j_WHITE
				    			     return (True, resetCalcState calcState)
						     else
			 if objMI == stop calcState  then return (True, stopCalcState calcState) else
			 if objMI == Main.print calcState then do j_print canvasobj
								  return (True, calcState)
							  else
			 if objCan == canvas
				  then do j_setnamedcolorbg canvasobj j_WHITE
					  return (True, calcState { x = -1,
								    y = -1 })
				  else return (True, calcState)
       state <- if dowork state
		     then do newstate <- return state { y = y state + 1 }
			     if y newstate >= width newstate
				 then return newstate { y = 0, dowork = False }
				 else let (rs, gs, bs) = makergbs newstate canvas [] [] []
				      in do
					 j_drawimagesource canvas 0 (y newstate) (width newstate) 1 rs gs bs
					 j_sync
					 return newstate
		     else return state
       if not again
	  then return state
	  else waitForFrameAction frame canvas state
   where canvasobj = (Object (unCanvas canvas))
       
mandel :: Double -> Double -> Double -> Double -> Int -> Int -> Int
mandel x y zre zim maxiter iter =
    if iter1 >= maxiter
       then maxiter
       else let x1  = x * x - y * y + zre
		y1  = 2 * x * y + zim
		in if x1*x1 + y1*y1 > 4.0 
		       then iter1
		       else mandel x1 y1 zre zim maxiter iter1
    where iter1 = iter+1

makergb :: CalcState -> (Int, Int, Int)
makergb cs = let zre = xstart cs + (fromIntegral (x cs)) * ( xend cs - xstart cs ) / (fromIntegral (width cs))
		 zim = ystart cs + (fromIntegral (y cs)) * ( yend cs - ystart cs ) / (fromIntegral (height cs))
		 it = mandel 0.0 0.0 zre zim 512 0
	     in (it * 11, it * 13, it * 17)

makergbs :: CalcState -> Canvas -> [Int] -> [Int] -> [Int] -> ([Int], [Int], [Int])
makergbs cs canvas rs gs bs = let ncs = cs { x = x cs + 1 }
			in if x ncs >= width ncs
		           then (rs, gs, bs)
		           else let (r,g,b) = makergb ncs
				in makergbs ncs canvas (rs ++ [r]) (gs ++ [g]) (bs ++ [b])
