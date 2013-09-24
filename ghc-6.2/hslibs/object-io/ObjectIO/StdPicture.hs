{-# OPTIONS -#include "OSWindows\Windows_C_12\cpicture_121.h" #-}

module StdPicture(module StdPictureDef, module StdPicture, Draw(..), doScreenDraw) where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	OSWindow contains OS operations to manage windows and controls.
--	********************************************************************************


--	Drawing functions and other operations on Picture

import	OSPicture
import	OSFont
import	OSRgn
import	CommonDef
import	StdPictureDef
import	StdIOBasic
import  Foreign.Ptr(nullPtr)

--	Pen attribute functions:

setPenAttributes :: [PenAttribute] -> Draw ()
setPenAttributes atts = mapM_ setAttribute atts
	where
	  setAttribute :: PenAttribute -> Draw ()
	  setAttribute (PenSize size) = setPenSize size
	  setAttribute (PenPos   pos) = setPenPos   pos
	  setAttribute (PenColour  c) = setPenColour  c
	  setAttribute (PenBack    c) = setPenBack    c
	  setAttribute (PenFont font) = setPenFont font

getPenAttributes :: Draw [PenAttribute]
getPenAttributes = do
	pen <- getPictPen
	return (getattribute pen)
	where
	  getattribute :: Pen -> [PenAttribute]
	  getattribute (Pen{penSize=psize,penForeColour=fc,penBackColour=bc,penPos=pos,penFont=font}) =
		  [PenSize psize,PenPos pos,PenColour fc,PenBack bc,PenFont font]


--	Pen position attributes:
setPenPos :: Point2 -> Draw ()
setPenPos = setPictPenPos

getPenPos :: Draw Point2
getPenPos = getPictPenPos

class MovePen f where
	movePenPos :: f -> Draw ()  -- Move the pen position as much as when drawing the figure.


instance MovePen Vector2 where
	movePenPos v = movePictPenPos v

instance MovePen Curve where	
	movePenPos curve = do
	  curPos <- getPictPenPos
	  let (_,_,endPos) = getCurveRectBeginEnd curPos curve
	  setPictPenPos endPos
	  return ()


-- PenSize attributes:
setPenSize :: Int -> Draw ()
setPenSize = setPictPenSize

getPenSize :: Draw Int
getPenSize = getPictPenSize

setDefaultPenSize :: Draw ()
setDefaultPenSize = setPictPenSize 1


--	Colour attributes:
setPenColour :: Colour -> Draw ()
setPenColour = setPictPenColour

getPenColour :: Draw Colour
getPenColour = getPictPenColour

setPenBack :: Colour -> Draw ()
setPenBack = setPictBackColour

getPenBack :: Draw Colour
getPenBack = getPictBackColour

setDefaultPenColour :: Draw ()
setDefaultPenColour = setPictPenColour Black

setDefaultPenBack :: Draw ()
setDefaultPenBack = setPictBackColour White


--	Font attributes:
setPenFont :: Font -> Draw ()
setPenFont = setPictPenFont

getPenFont :: Draw Font
getPenFont = getPictPenFont

setDefaultPenFont :: Draw ()
setDefaultPenFont = setPictPenDefaultFont


--	Font operations:
openFont :: FontDef -> Draw (Maybe Font)
openFont (FontDef{fName=name,fStyles=styles,fSize=size}) = liftIO (osSelectFont (name,styles,size))

openDefaultFont :: Draw Font
openDefaultFont = liftIO (osDefaultFont)

openDialogFont :: Draw Font
openDialogFont = liftIO (osDialogFont)

getFontNames :: Draw [FontName]
getFontNames = liftIO (osFontNames)

getFontStyles :: FontName -> Draw [FontStyle]
getFontStyles fName = liftIO (osFontStyles fName)

getFontSizes :: Int -> Int -> FontName -> Draw [FontSize]
getFontSizes sizeBound1 sizeBound2 fName = liftIO (osFontSizes sizeBound1 sizeBound2 fName)	

getFontDef :: Font -> FontDef
getFontDef font = FontDef{fName=name,fStyles=styles,fSize=size}
	where (name,styles,size) = osFontGetDef font

getFontCharWidth :: Font -> Char -> Draw Int
getFontCharWidth font char = do
	osPictContext <- peekOSPictContext
	widths <- liftIO (osGetFontCharWidths (Just osPictContext) [char] font)
	return (head widths)

getFontCharWidths :: Font -> [Char] -> Draw [Int]
getFontCharWidths font chars = do
	osPictContext <- peekOSPictContext
	liftIO (osGetFontCharWidths (Just osPictContext) chars font)

getFontStringWidth :: Font -> String -> Draw Int
getFontStringWidth font string = do
	osPictContext <- peekOSPictContext
	widths <- liftIO (osGetFontStringWidths (Just osPictContext) [string] font)
	return (head widths)

getFontStringWidths :: Font -> [String] -> Draw [Int]
getFontStringWidths font strings = do
	osPictContext <- peekOSPictContext
	liftIO (osGetFontStringWidths (Just osPictContext) strings font)

getFontMetrics :: Font -> Draw FontMetrics
getFontMetrics font = do
	osPictContext <- peekOSPictContext
	(ascent,descent,leading,maxwidth) <- liftIO (osGetFontMetrics (Just osPictContext) font)
	return (FontMetrics{fAscent=ascent,fDescent=descent,fLeading=leading,fMaxWidth=maxwidth})

getPenFontCharWidth :: Char -> Draw Int
getPenFontCharWidth char = do
	font <- getPenFont
	getFontCharWidth font char

getPenFontCharWidths :: [Char] -> Draw [Int]
getPenFontCharWidths chars = do
	font <- getPenFont
	getFontCharWidths font chars

getPenFontStringWidth :: String -> Draw Int
getPenFontStringWidth string = do
	font <- getPenFont
	getFontStringWidth font string

getPenFontStringWidths :: [String] -> Draw [Int]
getPenFontStringWidths strings = do
	font <- getPenFont
	getFontStringWidths font strings

getPenFontMetrics :: Draw FontMetrics
getPenFontMetrics = getPenFont >>= getFontMetrics


{-	Drawing functions.
	These functions are divided into the following classes:
	Drawables:
		draw     'line-oriented' figures at the current  pen position.
		drawAt   'line-oriented' figures at the argument pen position.
		undraw     f = appPicture (draw     f o setPenColour background)
		undrawAt x f = appPicture (drawAt x f o setPenColour background)
	Fillables:
		fill     'area-oriented' figures at the current  pen position.
		fillAt   'area-oriented' figures at the argument pen position.
		unfill     f = appPicture (fill     f o setPenColour background)
		unfillAt x f = appPicture (fillAt x f o setPenColour background)
	Hilites:
		hilite	 draws figures in the appropriate 'hilite' mode at the current pen position.
		hiliteAt draws figures in the appropriate 'hilite' mode at the current pen position.
		Both functions reset the 'hilite' after drawing.
-}

class Drawables figure where
	draw	:: figure -> Draw ()
	drawAt	:: Point2 -> figure -> Draw ()
	undraw	:: figure -> Draw ()
	undrawAt:: Point2 -> figure -> Draw ()

class Fillables figure where
	fill	:: figure -> Draw ()
	fillAt	:: Point2 -> figure -> Draw ()
	unfill	:: figure -> Draw ()
	unfillAt:: Point2 -> figure -> Draw ()

class Hilites figure where
	hilite	:: figure -> Draw ()
	hiliteAt:: Point2 -> figure -> Draw ()


{-	(app/acc)Picture applies the given drawing function to the given picture.
	When drawing is done, all picture attributes are set to the attribute values of the original picture.
-}
appPicture :: Draw a -> Draw a
appPicture drawf = do
	pen <- getPictPen
	x <- drawf
	setPictPen pen
	return x



--	Drawing in a clipping region.

data Region
   = Region
   	{ region_shape	:: [RegionShape]
	, region_bound	:: Rect
	}
data RegionShape
   = RegionRect	   Rect
   | RegionPolygon Point2 [Vector2]


isEmptyRegion :: Region -> Bool
isEmptyRegion (Region {region_shape=[]}) = True
isEmptyRegion _				 = False

getRegionBound :: Region -> Rectangle
getRegionBound (Region{region_bound=rect}) = rectToRectangle rect

class ToRegion area where
	toRegion :: area -> Region

data PolygonAt
   = PolygonAt
   	{ polygon_pos	:: Point2
	, polygon	:: Polygon
	}

instance ToRegion Rectangle where	
	toRegion rectangle
		| isEmptyRect rect	= zero
		| otherwise		= Region{region_shape=[RegionRect rect],region_bound=rect}
		where
			rect = rectangleToRect rectangle

instance ToRegion PolygonAt where
	toRegion (PolygonAt{polygon_pos=p@(Point2{x=x,y=y}),polygon=Polygon{polygon_shape=polygon_shape}})
		| isEmptyRect bound	= zero
		| otherwise		= Region{region_shape=[RegionPolygon p shape],region_bound=bound}
		where
		    shape	= closeShape zero polygon_shape
		    bound	= polyBound p shape (Rect{rleft=x,rtop=y,rright=x,rbottom=y})

		    polyBound :: Point2 -> [Vector2] -> Rect -> Rect
		    polybound _ [] bound = bound
		    polyBound p (v:vs) (Rect{rleft=minx,rtop=miny,rright=maxx,rbottom=maxy}) =
			    polyBound p' vs (Rect{rleft=minx',rtop=miny',rright=maxx',rbottom=maxy'})
			    where
				    p'@(Point2 {x=x,y=y}) = movePoint v p
				    minx'	= min minx x
				    miny'	= min miny y
				    maxx'	= max maxx x
				    maxy'	= max maxy y		

		    closeShape :: Vector2 -> [Vector2] -> [Vector2]
		    closeShape v (v':vs) = (v':closeShape (v+v') vs)
		    closeShape v []
			    | v==zero	= []
			    | otherwise	= [Vector2{vx=0-(vx v),vy=0-(vy v)}]


instance ToRegion area => ToRegion [area] where
	toRegion []		= zero
	toRegion (area:areas)	= sumRegion (toRegion area) (toRegion areas)
	

instance (ToRegion area1, ToRegion area2) => ToRegion (Tup area1 area2) where	
	toRegion (r1 :^: r2) = sumRegion (toRegion r1) (toRegion r2)

instance Zero Region where
	zero = Region{region_shape=[],region_bound=zero}
	

sumRegion :: Region -> Region -> Region
sumRegion r1 r2
	| isEmptyRect (region_bound r1) = r2
	| isEmptyRect (region_bound r2) = r1
	| otherwise = Region{region_shape=(region_shape r1)++(region_shape r2),
			     region_bound=sumBound (region_bound r1) (region_bound r2)}
		where
			sumBound :: Rect -> Rect -> Rect
			sumBound (Rect{rleft=minx,rtop=miny,rright=maxx,rbottom=maxy}) 
				 (Rect{rleft=minx',rtop=miny',rright=maxx',rbottom=maxy'}) =
			    Rect{rleft=min minx minx',rtop=min miny miny',rright=max maxx maxx',rbottom=max maxy maxy'}

accClipPicture :: Region -> Draw x -> Draw x
accClipPicture region drawf = do
	curClipRgn <- pictGetClipRgn
	newClipRgn <- liftIO osNewRgn
	context <- peekOSPictContext
	(hFac,vFac) <- liftIO (getPictureScalingFactors context)
	origin <- getPictOrigin
	newClipRgn <- liftIO (setRgnShapes hFac vFac origin (region_shape region) newClipRgn)	
	let (set,dispose) = if curClipRgn==nullPtr then (pictSetClipRgn,\_ -> return ()) else (pictAndClipRgn,osDisposeRgn)
	set newClipRgn
	x <- drawf
	pictSetClipRgn curClipRgn
	liftIO (dispose curClipRgn >> osDisposeRgn newClipRgn)
	return x
	where
	    setRgnShapes :: (Int,Int) -> (Int,Int) -> Point2 -> [RegionShape] -> OSRgnHandle -> IO OSRgnHandle
	    setRgnShapes hFac vFac origin (shape:shapes) rgn = do
		rgn <- setRgnShape hFac vFac origin shape rgn
		setRgnShapes hFac vFac origin shapes rgn
		where
		    setRgnShape :: (Int,Int) -> (Int,Int) -> Point2 -> RegionShape -> OSRgnHandle -> IO OSRgnHandle
		    setRgnShape hFac vFac (Point2{x=ox,y=oy}) (RegionRect (Rect {rleft=left,rtop=top,rright=right,rbottom=bottom})) rgn = do
			    rectRgn <- osNewRectRgn rect
			    sumRgn	<- osUnionRgn rectRgn rgn
			    osDisposeRgn rectRgn
			    osDisposeRgn rgn
			    return sumRgn
			    where
				    rect = Rect
					    { rleft   = scale hFac (left  -ox)
					    , rtop    = scale vFac (top   -oy)
					    , rright  = scale hFac (right -ox)
					    , rbottom = scale vFac (bottom-oy)
					    }
		    setRgnShape hFac vFac (Point2{x=ox,y=oy}) (RegionPolygon (Point2{x=x,y=y}) shape) rgn = do
			    emptyRgn <- osNewRgn
			    polyRgn  <- osPolyRgn (scale hFac (x-ox),scale vFac (y-oy)) (map (\(Vector2{vx=vx,vy=vy})->(scale hFac vx,scale vFac vy)) shape) emptyRgn
			    sumRgn <- osUnionRgn polyRgn rgn
			    osDisposeRgn polyRgn
			    osDisposeRgn rgn
			    return sumRgn
	    setRgnShapes _ _ _ _ rgn = return rgn

	    scale :: (Int,Int) -> Int -> Int
	    scale (n,d) x = n*x `div` d


{-	(app/acc)XorPicture applies the given drawing function to the given picture in the platform appropriate
	xor mode. 
-}

accXorPicture :: Draw a -> Draw a
accXorPicture drawf = do
	setPictXorMode
	x <- drawf
	setPictNormalMode
	return x


{-	Hiliting figures: -}

instance Hilites Box where
	hilite box = do
		setPictHiliteMode
		curPos <- getPictPenPos
		pictFillRect (boxToRect curPos box)
		setPictNormalMode
		
	hiliteAt base box = do
		setPictHiliteMode
		pictFillRect (boxToRect base box)
		setPictNormalMode

instance Hilites Rectangle where
	hilite rectangle = do
		setPictHiliteMode
		pictFillRect (rectangleToRect rectangle)
		setPictNormalMode		
	
	hiliteAt _ rectangle = do
		setPictHiliteMode
		pictFillRect (rectangleToRect rectangle)
		setPictNormalMode


drawPoint :: Draw ()
drawPoint = do
	curPos <- getPictPenPos
	pictDrawPoint curPos
	setPictPenPos curPos{x=(x curPos)+1}

drawPointAt :: Point2 -> Draw ()
drawPointAt point = do
	curPos <- getPictPenPos
	pictDrawPoint point
	setPictPenPos curPos


-- Point2 connecting drawing operations:

drawLineTo :: Point2 -> Draw ()
drawLineTo = pictDrawLineTo

drawLine :: Point2 -> Point2 -> Draw ()
drawLine = pictDrawLine


-- Text drawing operations:

instance Drawables Char where
	draw = pictDrawChar
	
	drawAt pos char = do
	  curPos <- getPictPenPos
	  setPictPenPos pos
	  pictDrawChar char
	  setPictPenPos curPos
	
	undraw = pictUndrawChar
	
	undrawAt pos char = do
	  curPos <- getPictPenPos
	  setPictPenPos pos
	  pictUndrawChar char
	  setPictPenPos curPos

instance Drawables String where
	draw = pictDrawString	
	
	drawAt pos string = do
	  curPos <- getPictPenPos
	  setPictPenPos pos
	  pictDrawString string
	  setPictPenPos curPos
	
	undraw = pictUndrawString	
	
	undrawAt pos string = do
	  curPos <- getPictPenPos
	  setPictPenPos pos
	  pictUndrawString string
	  setPictPenPos curPos


--	Line2 drawing operations:

instance Drawables Line2 where
	draw     (Line2 {line_end1=e1,line_end2=e2}) = pictDrawLine e1 e2	
	drawAt _ (Line2 {line_end1=e1,line_end2=e2}) = pictDrawLine e1 e2
	
	undraw     (Line2 {line_end1=e1,line_end2=e2}) = pictUndrawLine e1 e2	
	undrawAt _ (Line2 {line_end1=e1,line_end2=e2}) = pictUndrawLine e1 e2


--	Vector2 drawing operations:

instance Drawables Vector2 where
	draw (Vector2 {vx=vx,vy=vy}) = do
		curPos <- getPictPenPos
		let endPos = Point2{x=(x curPos)+vx,y=(y curPos)+vy}
		pictDrawLineTo endPos
	
	drawAt pos@(Point2{x=x,y=y}) (Vector2 {vx=vx,vy=vy}) =
		pictDrawLine pos (Point2 {x=x+vx,y=y+vy})
	
	undraw (Vector2 {vx=vx,vy=vy}) = do
		curPos <- getPictPenPos
		let endPos = Point2{x=(x curPos)+vx,y=(y curPos)+vy}
		pictUndrawLineTo endPos	
	
	undrawAt pos@(Point2{x=x,y=y}) (Vector2 {vx=vx,vy=vy}) =
		pictUndrawLine pos (Point2 {x=x+vx,y=y+vy})


--	Oval drawing operations:

instance Drawables Oval where
	draw oval = do
		curPos <- getPictPenPos
		pictDrawOval curPos oval
	
	drawAt = pictDrawOval
	
	undraw oval = do
		curPos <- getPictPenPos
		pictUndrawOval curPos oval
	
	undrawAt = pictUndrawOval



instance Fillables Oval where
	fill oval = do
		curPos <- getPictPenPos
		pictFillOval curPos oval
	
	fillAt = pictFillOval
	
	unfill oval = do
		curPos <- getPictPenPos
		pictUnfillOval curPos oval
	
	unfillAt = pictUnfillOval


--	Curve drawing operations:

instance Drawables Curve where
	draw curve = do
		curPos <- getPictPenPos
		pictDrawCurve True curPos curve
	
	drawAt = pictDrawCurve False
	
	undraw curve = do
		curPos <- getPictPenPos
		pictUndrawCurve True curPos curve	
	
	undrawAt = pictUndrawCurve False

instance Fillables Curve where
	fill curve = do
		curPos <- getPictPenPos
		pictFillCurve True curPos curve
	
	fillAt = pictFillCurve False

	unfill curve = do
		curPos <- getPictPenPos
		pictUnfillCurve True curPos curve
	
	unfillAt = pictUnfillCurve False


--	Box drawing operations:

instance Drawables Box where	
	draw box = do
		curPos <- getPictPenPos
		pictDrawRect (boxToRect curPos box)
	
	drawAt point box = pictDrawRect (boxToRect point box)
	
	undraw box = do
		curPos <- getPictPenPos
		pictUndrawRect (boxToRect curPos box)
	
	undrawAt point box = pictUndrawRect (boxToRect point box)

instance Fillables Box where
	fill box = do
		curPos <- getPictPenPos
		pictFillRect (boxToRect curPos box)
	
	fillAt pos box = pictFillRect (boxToRect pos box)

	unfill box = do
		curPos <- getPictPenPos
		pictUnfillRect (boxToRect curPos box)
	
	unfillAt pos box = pictUnfillRect (boxToRect pos box)

boxToRect :: Point2 -> Box -> Rect
boxToRect (Point2 {x=x,y=y}) (Box {box_w=bw,box_h=bh}) =
	Rect {rleft=l,rtop=t,rright=r,rbottom=b}
	where
	  (l,r) = minmax x (x+bw)
	  (t,b) = minmax y (y+bh)


--	Rectangle drawing operations:

instance Drawables Rectangle where
	draw rectangle = pictDrawRect (rectangleToRect rectangle)
	
	drawAt _ rectangle = pictDrawRect (rectangleToRect rectangle)

	undraw rectangle = pictUndrawRect (rectangleToRect rectangle)
	
	undrawAt _ rectangle = pictUndrawRect (rectangleToRect rectangle)

instance Fillables Rectangle where
	fill rectangle = pictFillRect (rectangleToRect rectangle)
	
	fillAt _ rectangle = pictFillRect (rectangleToRect rectangle)

	unfill rectangle = pictUnfillRect (rectangleToRect rectangle)
	
	unfillAt _ rectangle = pictUnfillRect (rectangleToRect rectangle)


--	Polygon drawing operations:

instance Drawables Polygon where
	draw polygon = do
		curPos <- getPictPenPos
		pictDrawPolygon curPos polygon
	
	drawAt = pictDrawPolygon

	undraw polygon = do
		curPos <- getPictPenPos
		pictUndrawPolygon curPos polygon
	
	undrawAt = pictUndrawPolygon

instance Fillables Polygon where
	fill polygon = do
		curPos <- getPictPenPos
		pictFillPolygon curPos polygon
	
	fillAt = pictFillPolygon
	
	unfill polygon = do
		curPos <- getPictPenPos
		pictUnfillPolygon curPos polygon
	
	unfillAt = pictUnfillPolygon


-- MW...
getResolution :: Draw (Int, Int)
getResolution = do
	context <- peekOSPictContext
	liftIO (getResolutionC context)
-- ... MW

--	Standard GUI object rendering function.

type Look = SelectState ->					-- Current SelectState of GUI object
	    UpdateState ->					-- The area to be rendered
	    Draw ()						-- The rendering action

stdUnfillNewFrameLook :: SelectState -> UpdateState -> Draw ()
stdUnfillNewFrameLook _ (UpdateState {newFrame=newFrame}) = unfill newFrame

stdUnfillUpdAreaLook :: SelectState -> UpdateState -> Draw ()
stdUnfillUpdAreaLook _ (UpdateState {updArea=updArea}) = mapM_ unfill updArea