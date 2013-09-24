{-# OPTIONS -#include "OSWindows\Windows_C_12\cpicture_121.h" #-}

module OSPicture ( Picture(..), Origin, OSPictContext, Pen(..), OSFont.Font
                 , peekOSPictContext
                 , defaultPen, dialogPen, setPenAttribute
                 , Draw(..), doDraw, doScreenDraw
                 , getPictOrigin, setPictOrigin, getPictPen, setPictPen, setPictPenPos, getPictPenPos
		 , movePictPenPos, setPictPenSize, getPictPenSize, setPictPenColour, setPictBackColour 
		 , getPictPenColour, getPictBackColour, setPictPenFont, getPictPenFont, setPictPenDefaultFont 
		 , setPictXorMode, setPictHiliteMode, setPictNormalMode, pictDrawPoint, pictDrawLineTo, pictUndrawLineTo 
		 , pictDrawLine, pictUndrawLine, pictDrawChar, pictUndrawChar, pictDrawString, pictUndrawString 
		 , pictDrawOval, pictUndrawOval, pictFillOval, pictUnfillOval, pictDrawCurve, pictUndrawCurve 
		 , pictFillCurve, pictUnfillCurve, pictDrawRect, pictUndrawRect, pictFillRect, pictUnfillRect 
		 , pictScroll, pictDrawPolygon, pictUndrawPolygon, pictFillPolygon, pictUnfillPolygon, pictGetClipRgn 
		 , pictSetClipRgn, pictAndClipRgn, getCurveRectBeginEnd, toRGBtriple
                 , module StdPictureDef, PictCCall_12.getResolutionC, PictCCall_12.getPictureScalingFactors
                 ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	OSPicture contains drawing functions and other operations on Pictures. 
--	********************************************************************************


import CommonDef
import OSRgn
import OSFont
import PictCCall_12
import StdIOBasic
import StdPictureDef
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(when)

data	Picture
	= Picture
		{ pictContext   :: !OSPictContext	-- The context for drawing operations
		, pictOrigin    :: !Origin		-- The current origin of the picture
		, pictPen       :: !Pen			-- The current state of the pen
		, pictToScreen  :: !Bool		-- Flag: the output goes to screen (True) or printer (False)
		}
		
type	Origin = Point2
type	OSPictContext = HDC

data  Pen
	= Pen
		{ penSize       :: !Int			-- The width and height of the pen
  		, penForeColour :: !Colour		-- The drawing colour of the pen
		, penBackColour :: !Colour		-- The background colour of the pen
		, penPos        :: !Point2		-- The pen position in local coordinates
		, penFont       :: !Font		-- The font information to draw text and characters
		}


--	Conversion operations to and from Picture

peekOSPictContext :: Draw OSPictContext
peekOSPictContext = Draw (\picture ->
	return (pictContext picture,picture))
	
defaultPen :: Pen
defaultPen
	= Pen
		{ penSize       = 1
		, penForeColour = Black
		, penBackColour = White
		, penPos        = Point2 {x=0,y=0}
		, penFont       = defaultFont
		}
	where
		defaultFont     = unsafePerformIO osDefaultFont

dialogPen :: Pen
dialogPen
	= Pen
		{ penSize       = 1
		, penForeColour = Black
		, penBackColour = White
		, penPos        = Point2 {x=0,y=0}
		, penFont       = dialogFont
		}
	where
		dialogFont      = unsafePerformIO osDialogFont

setPenAttribute :: PenAttribute -> Pen -> Pen
setPenAttribute (PenSize   size)   pen = pen {penSize      =max 1 size}
setPenAttribute (PenPos    pos)    pen = pen {penPos       =pos       }
setPenAttribute (PenColour colour) pen = pen {penForeColour=colour    }
setPenAttribute (PenBack   colour) pen = pen {penBackColour=colour    }
setPenAttribute (PenFont   font)   pen = pen {penFont      =font      }


-- Drawing monads

newtype Draw a = Draw (Picture -> IO (a, Picture))

instance Monad Draw where
   return x = Draw (\pict -> return (x, pict))
   (Draw f) >>= g = Draw (\pict -> do
   	(x, pict) <- f pict
   	let (Draw f2) = g x
   	f2 pict)
   	
instance  Functor Draw where
   fmap f x = x >>= (return . f)

instance IOMonad Draw where
	liftIO f = Draw (\picture -> f >>= \x -> return (x, picture))

doDraw :: Origin -> Pen -> Bool -> OSPictContext -> Draw a -> IO (a, Origin, Pen, Bool)
doDraw origin pen@(Pen {penSize=penSize,penForeColour=penForeColour,penBackColour=penBackColour,penPos=penPos,penFont=penFont}) isScreenOutput context (Draw drawf) = do
    winInitPicture
	      penSize
	      iModeCopy
	      initforecolour
	      initbackcolour
	      initpen
	      (osfontname fontimp,osfontstyles fontimp,osfontsize fontimp)
	      (0,0)
	      context
    (res, picture) <- drawf (Picture
	    { pictContext = context
	    , pictOrigin  = origin
	    , pictPen     = pen
	    , pictToScreen= isScreenOutput
	    }
	   )
    (_,_,_,_,_,_) <- winDonePicture context;
    return (res, pictOrigin picture,pictPen picture,pictToScreen picture)
    where
	fontimp        = osFontGetImp penFont
	initforecolour = toRGBtriple penForeColour
	initbackcolour = toRGBtriple penBackColour
	initpen        = toTuple (penPos-origin)
		
doScreenDraw :: Draw x -> IO x
doScreenDraw f = do
	hdc <- winCreateScreenHDC
	(x,_,_,_) <- doDraw zero defaultPen True hdc f	
	winDestroyScreenHDC hdc
	return x


-- Attribute functions.

--	Access to Origin and Pen:
getPictOrigin :: Draw Origin
getPictOrigin = Draw (\picture -> return (pictOrigin picture,picture))

setPictOrigin :: Origin -> Draw ()
setPictOrigin origin = Draw (\picture -> return ((), picture{pictOrigin=origin}))

getPictPen :: Draw Pen
getPictPen = Draw (\picture -> return (pictPen picture,picture))

setPictPen :: Pen -> Draw ()
setPictPen pen = do
	setPictPenSize    (penSize pen)
	setPictPenColour  (penForeColour pen)
	setPictBackColour (penBackColour pen)
	setPictPenPos     (penPos pen)
	setPictPenFont    (penFont pen)


--	Change the pen position:
setPictPenPos :: Point2 -> Draw ()
setPictPenPos newpos = Draw (\picture@Picture{pictOrigin=origin,pictPen=pen@(Pen {penPos=pos}),pictContext=context} ->
	if newpos == pos then return ((), picture)
	else do
		winMovePenTo (toTuple (newpos-origin)) context		  
		return ((), picture{pictPen=pen{penPos=newpos}}))

getPictPenPos :: Draw Point2
getPictPenPos = Draw (\picture -> return (penPos (pictPen picture),picture))

movePictPenPos :: Vector2 -> Draw ()
movePictPenPos v@(Vector2{vx=vx,vy=vy}) = Draw (\picture@(Picture{pictPen=pen@(Pen{penPos=Point2{x=x,y=y}}),pictContext=context}) -> do
	winMovePen (toTuple v) context
	return ((), picture{pictPen=pen{penPos=Point2{x=x+vx,y=y+vy}}}))

--	Change the pen size:
setPictPenSize :: Int -> Draw ()
setPictPenSize w = Draw (\picture@(Picture {pictContext=context,pictPen=pen}) ->
	let w' = max 1 w
	in  if w' == penSize pen then return ((), picture)
	    else do
		    winSetPenSize w' context		  
		    return ((), picture{pictPen=pen{penSize=w'}}))


getPictPenSize :: Draw Int
getPictPenSize = Draw (\picture -> return (penSize (pictPen picture),picture))


--	Change the PenColour:
setPictPenColour :: Colour -> Draw ()
setPictPenColour colour = Draw (\picture@(Picture {pictPen=pen,pictContext=context}) ->
	let
	    reqRGB = toRGBtriple colour
	    curRGB = toRGBtriple (penForeColour pen)
	in	
	    if reqRGB == curRGB then return ((), picture)
	    else do
		    winSetPenColor reqRGB context
		    return ((), picture{pictPen=pen{penForeColour=colour}}))
	

setPictBackColour :: Colour -> Draw ()
setPictBackColour colour = Draw (\picture@(Picture {pictPen=pen,pictContext=context}) ->
	let
	     reqRGB = toRGBtriple colour
	     curRGB = toRGBtriple (penBackColour pen)
	in
	     if reqRGB == curRGB then return ((), picture)
	     else do
		     winSetBackColor (toRGBtriple colour) context
		     return ((), picture{pictPen=pen{penBackColour=colour}}))
	

toRGBtriple :: Colour -> (Int,Int,Int)
toRGBtriple rgb@(RGB {}) = (setBetween (r rgb) minRGB maxRGB,setBetween (g rgb) minRGB maxRGB,setBetween (b rgb) minRGB maxRGB)
toRGBtriple Black     = (minRGB,minRGB,minRGB)
toRGBtriple DarkGrey  = (round((fromIntegral maxRGB)/4.0), round((fromIntegral maxRGB)/4.0), round((fromIntegral maxRGB)/4.0))
toRGBtriple Grey      = (round((fromIntegral maxRGB)/2.0), round((fromIntegral maxRGB)/2.0), round((fromIntegral maxRGB)/2.0))
toRGBtriple LightGrey = (round((fromIntegral maxRGB)*0.75),round((fromIntegral maxRGB)*0.75),round((fromIntegral maxRGB)*0.75))
toRGBtriple White     = (maxRGB,maxRGB,maxRGB)
toRGBtriple Red       = (maxRGB,minRGB,minRGB)
toRGBtriple Green     = (minRGB,maxRGB,minRGB)
toRGBtriple Blue      = (minRGB,minRGB,maxRGB)
toRGBtriple Cyan      = (minRGB,maxRGB,maxRGB)
toRGBtriple Magenta   = (maxRGB,minRGB,maxRGB)
toRGBtriple Yellow    = (maxRGB,maxRGB,minRGB)


getPictPenColour :: Draw Colour
getPictPenColour = Draw (\picture -> return (penForeColour (pictPen picture), picture))

getPictBackColour :: Draw Colour
getPictBackColour = Draw (\picture -> return (penBackColour (pictPen picture), picture))


--	Change the font attributes:
setPictPenFont :: Font -> Draw ()
setPictPenFont font = Draw (\picture@(Picture{pictContext=context,pictPen=pen}) ->
	let 
		imp = osFontGetImp font
		OSFont {osfontname=osfontname,osfontstyles=osfontstyles,osfontsize=osfontsize}	= imp
	in
	  if imp == osFontGetImp (penFont pen) then return ((), picture)
	  else do
		  winSetFont (osfontname,osfontstyles,osfontsize) context
		  return ((), picture{pictPen=pen{penFont=font}}))
	

getPictPenFont :: Draw Font
getPictPenFont = Draw (\picture -> return (penFont (pictPen picture),picture))

setPictPenDefaultFont :: Draw ()
setPictPenDefaultFont = Draw (\picture@(Picture{pictContext=context,pictPen=pen}) -> do
	font <- osDefaultFont
	let OSFont{osfontname=osfontname,osfontstyles=osfontstyles,osfontsize=osfontsize} = osFontGetImp font
	winSetFont (osfontname,osfontstyles,osfontsize) context	
	return ((), picture{pictPen=pen{penFont=font}}))


--	Drawing mode setting functions.

setPictXorMode :: Draw ()
setPictXorMode = Draw (\picture@(Picture {pictContext=context}) -> do
	winSetMode iModeXor context
	return ((), picture))
	
setPictHiliteMode :: Draw ()
setPictHiliteMode = Draw (\picture@(Picture {pictContext=context}) -> do
	winSetMode iModeXor context
	return ((), picture))
	
setPictNormalMode :: Draw ()
setPictNormalMode = Draw (\picture@(Picture {pictContext=context}) -> do
	winSetMode iModeCopy context
	return ((), picture))


{-	Point2 drawing operations.
	pictDrawPoint
		only draws a point at that position. The pen position is not changed.
-}
pictDrawPoint :: Point2 -> Draw ()
pictDrawPoint pos = Draw (\picture@(Picture{pictPen=pen,pictOrigin=origin,pictContext=context}) ->
	let psize = penSize pen		
	    (x',y') = toTuple (pos-origin)
	in do
	   (if psize==1
	    then winDrawPoint (x',y')
	    else winFillRectangle (Rect{rleft=x',rtop=y',rright=x'+psize,rbottom=y'+psize})) context
	   return ((), picture))



{-	Line drawing operations.
	pictDrawLineTo
		draws a line from the current pen position to the given pen position. 
		The new pen position is the endpoint of the line.	
	pictDrawLine
		draws a line from the first point to the second point. The pen position
		is not changed.
-}
pictDrawLineTo :: Point2 -> Draw ()
pictDrawLineTo pos = Draw (\picture@(Picture{pictOrigin=origin,pictContext=context,pictPen=pen}) -> do
	winLinePenTo (toTuple (pos-origin)) context
	return ((), picture{pictPen=pen{penPos=pos}}))

pictUndrawLineTo :: Point2 -> Draw ()
pictUndrawLineTo pos = Draw (\picture@(Picture {pictOrigin=origin,pictContext=context,pictPen=pen}) -> do
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winLinePenTo (toTuple (pos-origin)) context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	return ((), picture{pictPen=pen{penPos=pos}}))

pictDrawLine :: Point2 -> Point2 -> Draw ()
pictDrawLine a b = Draw (\picture@(Picture {pictContext=context,pictOrigin=origin}) -> do
	winDrawLine (toTuple (a-origin)) (toTuple (b-origin)) context
	return ((), picture))

pictUndrawLine :: Point2 -> Point2 -> Draw ()
pictUndrawLine a b = Draw (\picture@(Picture {pictOrigin=origin,pictContext=context,pictPen=pen}) -> do
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winDrawLine (toTuple (a-origin)) (toTuple (b-origin)) context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	return ((), picture))


{-	Text drawing operations.
	pictdraw(char/string) draws a char/string at the current pen position. The new
		pen position is immediately after the drawn char/string.
-}
pictDrawChar :: Char -> Draw ()
pictDrawChar char = Draw (\picture@(Picture {pictOrigin=origin,pictContext=context,pictPen=pen}) -> do
	winDrawChar char context
	(x,y) <- winGetPenPos context
	let Point2{x=ox,y=oy} = origin
	return ((), picture{pictPen=pen{penPos=Point2{x=ox+x,y=oy+y}}}))

pictUndrawChar :: Char -> Draw ()
pictUndrawChar char = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winDrawChar char context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	(x,y) <- winGetPenPos context
	let Point2{x=ox,y=oy} = origin
	return ((), picture{pictPen=pen{penPos=Point2{x=x+ox,y=y+oy}}}))

pictDrawString :: String -> Draw ()
pictDrawString string = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	winDrawString string context
	(x,y) <- winGetPenPos context
	let Point2{x=ox,y=oy} = origin
	return ((), picture{pictPen=pen{penPos=Point2{x=x+ox,y=y+oy}}}))

pictUndrawString :: String -> Draw ()
pictUndrawString string = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winDrawString string context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	(x,y) <- winGetPenPos context
	let Point2{x=ox,y=oy} = origin
	return ((), picture{pictPen=pen{penPos=Point2{x=x+ox,y=y+oy}}}))


{-	Oval drawing operations.
	pict(draw/fill)oval center oval 
		draws/fills an oval at center with horizontal and vertical radius. The new
		pen position is not changed.
-}
pictDrawOval :: Point2 -> Oval -> Draw ()
pictDrawOval center oval = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let rect = ovalToRect (center-origin) oval
	winDrawOval rect context
	return ((), picture))


pictUndrawOval :: Point2 -> Oval -> Draw ()
pictUndrawOval center oval = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	let rect = ovalToRect (center-origin) oval
	winDrawOval rect context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	return ((), picture))


pictFillOval :: Point2 -> Oval -> Draw ()
pictFillOval center oval = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	let rect = ovalToRect (center-origin) oval
	winFillOval rect context
	return ((), picture))


pictUnfillOval :: Point2 -> Oval -> Draw ()
pictUnfillOval center oval = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let rect = ovalToRect (center-origin) oval
	winEraseOval rect context
	return ((), picture))


ovalToRect :: Point2 -> Oval -> Rect
ovalToRect (Point2{x=x,y=y}) (Oval{oval_rx=oval_rx,oval_ry=oval_ry}) =
	Rect{rleft=x-rx,rtop=y-ry,rright=x+rx,rbottom=y+ry}
	where
	   rx = abs oval_rx
	   ry = abs oval_ry


{-	Curve drawing operations.
	pict(Draw/Fill)curve movePen point curve
		draws/fills a curve starting at point with a shape defined by curve. If movePen
		is True, then the new pen position is at the end of the curve, otherwise it does
		not change.
-}
pictDrawCurve :: Bool -> Point2 -> Curve -> Draw ()
pictDrawCurve movePen start curve = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let start' = start-origin
	let (wrect,wstart,wend)	= getCurveRectBeginEnd start' curve
	let end	= wend+origin
	winDrawCurve wrect (toTuple wstart) (toTuple wend) context
	when (movePen && end /= penPos pen) (winMovePenTo (toTuple wend) context)
	return ((), picture{pictPen=pen{penPos=end}}))


pictUndrawCurve :: Bool -> Point2 -> Curve -> Draw ()
pictUndrawCurve movePen start curve = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let start' = start-origin
	let (wrect,wstart,wend)	= getCurveRectBeginEnd start' curve
	let end = wend+origin
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winDrawCurve wrect (toTuple wstart) (toTuple wend) context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	when (movePen && end /= penPos pen) (winMovePenTo (toTuple wend) context)
	return ((), picture{pictPen=pen{penPos=end}}))
	

pictFillCurve :: Bool -> Point2 -> Curve -> Draw ()
pictFillCurve movePen start curve = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let start' = start-origin
	let (wrect,wstart,wend)	= getCurveRectBeginEnd start' curve
	let end	= wend+origin
	winFillWedge wrect (toTuple wstart) (toTuple wend) context
	when (movePen && end /= penPos pen) (winMovePenTo (toTuple wend) context)
	return ((), picture{pictPen=pen{penPos=end}}))
	

pictUnfillCurve :: Bool -> Point2 -> Curve -> Draw ()
pictUnfillCurve movePen start curve = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	let start' = start-origin
	let (wrect,wstart,wend)	= getCurveRectBeginEnd start' curve
	let end	= wend+origin
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winFillWedge wrect (toTuple wstart) (toTuple wend) context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	when (movePen && end /= penPos pen) (winMovePenTo (toTuple wend) context)
	return ((), picture{pictPen=pen{penPos=end}}))
	

getCurveRectBeginEnd :: Point2 -> Curve -> (Rect,Point2,Point2)
getCurveRectBeginEnd start@(Point2{x=x,y=y}) (Curve{curve_oval=Oval{oval_rx=rx,oval_ry=ry},curve_from=from,curve_to=to,curve_clockwise=clockwise})
	| clockwise	= (rect,end,start)
	| otherwise	= (rect,start,end)
	where
	   rx'		= fromIntegral (abs rx)
	   ry'		= fromIntegral (abs ry)
	   cx		= x  - (round ((cos from)*rx'))
	   cy		= y  + (round ((sin from)*ry'))
	   ex		= cx + (round ((cos to  )*rx'))
	   ey		= cy - (round ((sin to  )*ry'))
	   end		= Point2{x=ex,y=ey}
	   rect		= Rect{rleft=cx-rx,rtop=cy-ry,rright=cx+rx,rbottom=cy+ry}


{-	Rect drawing operations.
	pict(draw/fill)rect rect
		draws/fills a rect. The pen position is not changed.
-}
pictDrawRect :: Rect -> Draw ()
pictDrawRect r = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	winDrawRectangle (subVector (toVector origin) r) context
	return ((), picture))

pictUndrawRect :: Rect -> Draw ()
pictUndrawRect r = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winDrawRectangle (subVector (toVector origin) r) context
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	return ((), picture))

pictFillRect :: Rect -> Draw ()
pictFillRect r = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	winFillRectangle (subVector (toVector origin) r) context
	return ((), picture))

pictUnfillRect :: Rect -> Draw ()
pictUnfillRect r = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	winEraseRectangle (subVector (toVector origin) r) context
	return ((), picture))


{-	Scrolling operation (handle with care).
-}
pictScroll :: Rect -> Vector2 -> Draw Rect
pictScroll r v = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	updRect <- winScrollRectangle (subVector (toVector origin) r) (toTuple v) context
	return (updRect, picture))

{-	Polygon drawing operations.
	pict(Draw/Fill)polygon point polygon
		draws/fills a polygon starting at point. The pen position is not changed.
-}
pictDrawPolygon :: Point2 -> Polygon -> Draw ()
pictDrawPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape
	winDrawPolygon context
	winEndPolygon
	return ((), picture))

pictUndrawPolygon :: Point2 -> Polygon -> Draw ()
pictUndrawPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winDrawPolygon context
	winEndPolygon
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	return ((), picture))

pictFillPolygon :: Point2 -> Polygon -> Draw ()
pictFillPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape
	winSetPenSize 1 context
	winFillPolygon context
	winDrawPolygon context
	winSetPenSize (penSize pen) context
	winEndPolygon
	return ((), picture))

pictUnfillPolygon :: Point2 -> Polygon -> Draw ()
pictUnfillPolygon start (Polygon{polygon_shape=shape}) = Draw (\picture@(Picture{pictContext=context,pictPen=pen,pictOrigin=origin}) -> do
	transferPolygon (start-origin) shape
	winSetPenColor (toRGBtriple (penBackColour pen)) context
	winSetPenSize 1 context
	winFillPolygon  context
	winDrawPolygon  context
	winSetPenSize (penSize pen) context
	winEndPolygon
	winSetPenColor (toRGBtriple (penForeColour pen)) context
	return ((), picture))

transferPolygon :: Point2 -> [Vector2] -> IO ()
transferPolygon start vs = do
	winStartPolygon (1 + length vs)
	winAddPolygonPoint wstart
	transferShape wstart vs
	where
	  wstart	= toTuple start

	  transferShape :: (Int,Int) -> [Vector2] -> IO ()
	  transferShape (x,y) ((Vector2 {vx=vx,vy=vy}):vs) = do
	  	winAddPolygonPoint newpos
	  	transferShape newpos vs
	  	where
	  		newpos = (x+vx,y+vy)
	  transferShape _ [] = return ()

{-	Clipping operations.
	pictGetClipRgn gets the current clipping region.
	pictSetClipRgn sets the given clipping region.
	pictAndClipRgn takes the intersection of the current clipping region and the argument region.
-}
pictGetClipRgn :: Draw OSRgnHandle
pictGetClipRgn = Draw (\picture@(Picture{pictContext=context}) -> do
	clipRgn <- winGetClipRgnPicture context
	return (clipRgn, picture))

pictSetClipRgn :: OSRgnHandle -> Draw ()
pictSetClipRgn clipRgn = Draw (\picture@(Picture{pictContext=context}) -> do
	winSetClipRgnPicture clipRgn context
	return ((), picture))

pictAndClipRgn :: OSRgnHandle -> Draw ()
pictAndClipRgn clipRgn = Draw (\picture@(Picture{pictContext=context}) -> do
	winClipRgnPicture clipRgn context
	return ((), picture))
