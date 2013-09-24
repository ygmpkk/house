module GraphicsPicture
	( arc, ellipse, shearEllipse
	, line, polyline, polygon
-- not in X11:	, polyBezier
        , text
        , textInfo
	) where

import qualified Xlib as X
import GraphicsDC
import GraphicsWindow(Graphic)
import Concurrent(readMVar)

----------------------------------------------------------------
-- The Interface (SOE, p50)
--
-- ellipses are specified by 2 opposite corners of the bounding box
-- arcs are segments of unfilled ellipses, from the start angle
--  counter-clockwise to the end angle.
-- (we could support filled arcs as well (useful for pie-charts))
-- shearEllipses are specified by 3 corners of the bounding parallelogram
--   listed in the order p0 p1 p2 where p0<->p1 and p0<->p2 are sides of
--   the parallelogram.
--   Note that the major and minor axes of the ellipse are the same as
--   the diagonals of the parallelogram.
----------------------------------------------------------------

arc          :: Point -> Point -> Angle -> Angle -> Graphic  -- unfilled
ellipse      :: Point -> Point           	 -> Graphic  -- filled
shearEllipse :: Point -> Point -> Point  	 -> Graphic  -- filled
line         :: Point -> Point           	 -> Graphic  -- unfilled
polyline     :: [Point]                          -> Graphic  -- unfilled
polygon      :: [Point]                          -> Graphic  -- filled
-- not in X11: polyBezier   :: [Point]           -> Graphic  -- unfilled
text         :: Point -> String                  -> Graphic  -- filled

-- textInfo s returns:
--
-- 1) The offset at which the string would be drawn according to the
--    current text alignment (e.g., (Center, Baseline) will result in
--    an offset of (-width/2,0))
--
-- 2) The size at which the text would be drawn using the current font.
--
textInfo :: String -> Draw ((Point,Size))

----------------------------------------------------------------
-- The Implementation
----------------------------------------------------------------

arc (x0,y0) (x1,y1) s e = mkDraw (\ dc -> X.fillArc (disp dc) (drawable dc) (paintGC dc) x' y' w' h' s' e')
 where
  (x,w) = minAndDelta x0 x1
  (y,h) = minAndDelta y0 y1
  x' = fromIntegral x
  y' = fromIntegral y
  w' = fromIntegral w
  h' = fromIntegral h
  s' = round (s * 64)
  e' = round (e * 64)

ellipse (x0,y0) (x1,y1) = mkDraw (\ dc -> X.fillArc (disp dc) (drawable dc) (brushGC dc) x' y' w' h' 0 threeSixty)
 where
  (x,w) = minAndDelta x0 x1
  (y,h) = minAndDelta y0 y1
  x' = fromIntegral x
  y' = fromIntegral y
  w' = fromIntegral w
  h' = fromIntegral h

-- X measures angles in 64ths of a degree
threeSixty :: Int
threeSixty = 360*64

shearEllipse p0 p1 p2 = mkDraw (\ dc -> X.fillPolygon (disp dc) (drawable dc) (brushGC dc) pts X.convex X.coordModeOrigin)
 where
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1
  (x2,y2) = fromPoint p2

  x = avg x1 x2 -- centre of parallelogram
  y = avg y1 y2
  
  dx1 = fromIntegral ((x1 - x0) `div` 2) -- distance to corners from centre
  dy1 = fromIntegral ((y1 - y0) `div` 2)
  dx2 = fromIntegral ((x2 - x0) `div` 2)
  dy2 = fromIntegral ((y2 - y0) `div` 2)

  pts = [ (x + round(c*dx1 + s*dx2), y + round(c*dy1 + s*dy2))
        | (c,s) <- cos'n'sins
        ]

cos'n'sins :: [(Double,Double)]
cos'n'sins = [ (cos a, sin a) | a <- angles ]

angles :: [Angle]
angles = take 40 [0, pi/20 .. ]

line p0 p1 = mkDraw (\ dc -> X.drawLine (disp dc) (drawable dc) (paintGC dc) x0 y0 x1 y1)
 where
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1

polyline pts = mkDraw (\ dc -> X.drawLines (disp dc) (drawable dc) (paintGC dc) (map fromPoint pts) X.coordModeOrigin)
polygon  pts = mkDraw (\ dc -> X.fillPolygon (disp dc) (drawable dc) (brushGC dc) (map fromPoint pts) X.complex X.coordModeOrigin)

text p s = mkDraw (\ dc -> do
  bs <- readMVar (ref_bits dc)
  let
    f       = font bs
    (halign, valign) = textAlignment bs

    width   = X.textWidth f s
    ascent  = X.ascentFromFontStruct f
    descent = X.descentFromFontStruct f

    x' = case halign of
         Left'  -> x
         Center -> x - width `div` 2
         Right' -> x - width + 1
    y' = case valign of
         Top      -> y + ascent
         Baseline -> y
         Bottom   -> y - descent + 1

  draw (bkMode bs) (disp dc) (drawable dc) (textGC dc) x' y' s
 )
 where
  (x,y) = fromPoint p

  -- Win32's DeviceContext has a BkMode in it.  In X, we call two different
  -- routines depending on what mode we want.
  draw Transparent = X.drawString
  draw Opaque      = X.drawImageString

textInfo s = mkDraw (\ dc -> do
  bs <- readMVar (ref_bits dc)
  let
    f       = font bs
    (halign, valign) = textAlignment bs

    width   = X.textWidth f s
    ascent  = X.ascentFromFontStruct f
    descent = X.descentFromFontStruct f

    x' = case halign of
         Left'  -> 0
         Center -> - width `div` 2
         Right' -> - width + 1
    y' = case valign of
         Top      -> ascent
         Baseline -> 0
         Bottom   -> - descent + 1

  return (toPoint (x',y'), toSize (width, ascent+descent)))

----------------------------------------------------------------
-- End
----------------------------------------------------------------
