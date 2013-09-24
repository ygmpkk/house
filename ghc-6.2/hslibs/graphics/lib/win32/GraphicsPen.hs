module GraphicsPen
	( Style(..)
	, Pen
	, createPen, deletePen, selectPen
	) where

import GraphicsTypes
import qualified Win32

----------------------------------------------------------------
newtype Pen = Pen Win32.HPEN

data Style
  = Solid 
  | Dash	  -- "-------"
  | Dot		  -- "......."	
  | DashDot	  -- "_._._._"	
  | DashDotDot	  -- "_.._.._"	
  | Null
  | InsideFrame

createPen :: Style -> Int -> RGB -> IO Pen
deletePen :: Pen -> IO ()
selectPen :: Pen -> Draw Pen
----------------------------------------------------------------

style :: Style -> Win32.PenStyle
style Solid       = Win32.pS_SOLID       
style Dash	  = Win32.pS_DASH        
style Dot	  = Win32.pS_DOT         
style DashDot	  = Win32.pS_DASHDOT     
style DashDotDot  = Win32.pS_DASHDOTDOT  
style Null	  = Win32.pS_NULL        
style InsideFrame = Win32.pS_INSIDEFRAME 

createPen sty width c = 
  Win32.createPen (style sty) (fromIntegral width) (fromRGB c) >>= return . Pen

deletePen (Pen pen) = 
  Win32.deletePen pen

selectPen (Pen p) = mkDraw (\hdc -> do
  p' <- Win32.selectPen hdc p
  return (Pen p'))

----------------------------------------------------------------
-- The end
----------------------------------------------------------------
