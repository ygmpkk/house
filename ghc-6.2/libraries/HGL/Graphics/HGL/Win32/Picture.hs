module Graphics.HGL.Win32.Picture
	( arc, ellipse, shearEllipse, line
	, polyline, polygon, polyBezier
	) where

import Graphics.HGL.Win32.Types
import qualified System.Win32 as Win32

----------------------------------------------------------------

arc          :: Point -> Point -> Angle -> Angle -> Draw ()
ellipse      :: Point -> Point          -> Draw ()
shearEllipse :: Point -> Point -> Point -> Draw ()
line         :: Point -> Point          -> Draw ()

polyline     :: [Point] -> Draw ()
polygon      :: [Point] -> Draw ()
polyBezier   :: [Point] -> Draw ()

----------------------------------------------------------------

arc p0 p1 start end = mkDraw (\ hdc -> Win32.arc hdc x0 y0 x1 y1 xs ys xe ye)
 where 
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1
  x = (x0 + x1) `div` 2
  y = (y0 + y1) `div` 2
  start' = 2 * pi * start / 360
  end'   = 2 * pi * end   / 360
  xs = x + round (100 * cos start')
  ys = y + round (100 * sin start')
  xe = x + round (100 * cos end')
  ye = y + round (100 * sin end')

ellipse p0 p1  = mkDraw (\ hdc -> Win32.ellipse hdc x0 y0 x1 y1)
 where 
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1

shearEllipse p0 p1 p2 = mkDraw (\ hdc -> 
  Win32.transformedEllipse hdc (fromPoint p0) (fromPoint p1) (fromPoint p2))

line p0 p1 = mkDraw (\ hdc -> Win32.moveToEx hdc x0 y0 >> Win32.lineTo   hdc x1 y1)
 where 
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1

polyline pts   = mkDraw (\ hdc -> Win32.polyline   hdc (map fromPoint pts))
polygon pts    = mkDraw (\ hdc -> Win32.polygon    hdc (map fromPoint pts))
polyBezier pts = mkDraw (\ hdc -> Win32.polyBezier hdc (map fromPoint pts))

----------------------------------------------------------------
