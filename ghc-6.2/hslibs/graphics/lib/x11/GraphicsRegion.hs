module GraphicsRegion
	( Region
	, emptyRegion, rectangleRegion, ellipseRegion, polygonRegion
	, intersectRegion, unionRegion, subtractRegion, xorRegion
	, regionToGraphic
	) where

import qualified Xlib as X
import GraphicsDC(DC(..), Draw, mkDraw, Point, fromPoint, Angle)
import GraphicsWindow(Graphic)
import IOExts( unsafePerformIO )

----------------------------------------------------------------
-- The Interface (SOE, p136)
--
-- Note that Win32 does not include emptyRegion (SOE, p140).
-- The obvious Win32 implementation (an empty rectangle) could create problems
-- when you calculate the bounding box
-- (This could be fixed by implementing Empty Regions explicitly in Haskell
--  at the (small) cost of an extra test on every region operation.)
----------------------------------------------------------------

type Region = X.Region

emptyRegion     :: Region
rectangleRegion :: Point -> Point -> Region
ellipseRegion   :: Point -> Point -> Region
polygonRegion   :: [Point] -> Region

intersectRegion :: Region -> Region -> Region
unionRegion     :: Region -> Region -> Region
subtractRegion  :: Region -> Region -> Region
xorRegion    	:: Region -> Region -> Region
			   
regionToGraphic :: Region -> Graphic

----------------------------------------------------------------
-- The Implementation
----------------------------------------------------------------

emptyRegion = unsafePerformIO X.createRegion

rectangleRegion (x0,y0) (x1,y1) = 
  polygonRegion [(x0,y0),(x0,y1),(x1,y1),(x1,y0)]
  
ellipseRegion p0 p1 = unsafePerformIO $ do
  X.polygonRegion pts X.evenOddRule
 where
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1

  rx = (x1 - x0) `div` 2
  ry = (y1 - y0) `div` 2

  cx = x0 + rx 
  cy = y0 + ry

  rx' = fromIntegral rx
  ry' = fromIntegral ry

  pts = [ (cx + round (rx' * c), cy + round (ry' * s))
        | (c,s) <- cos'n'sins
        ]

cos'n'sins :: [(Double,Double)]
cos'n'sins = [ (cos a, sin a) | a <- angles ]

angles :: [Angle]
angles = take 40 [0, pi/20 .. ]


polygonRegion pts = unsafePerformIO $ do
  X.polygonRegion (map fromPoint pts) X.evenOddRule

intersectRegion r1 r2 = unsafePerformIO $ do
  r <- X.createRegion
  X.intersectRegion r1 r2 r
  return r

unionRegion r1 r2 = unsafePerformIO $ do
  r <- X.createRegion
  X.unionRegion r1 r2 r
  return r

subtractRegion r1 r2 = unsafePerformIO $ do
  r <- X.createRegion
  X.subtractRegion r1 r2 r
  return r

xorRegion r1 r2 = unsafePerformIO $ do
  r <- X.createRegion
  X.xorRegion r1 r2 r
  return r

regionToGraphic r = mkDraw (\ dc -> do
  X.setRegion (disp dc) (brushGC dc) r
  X.fillRectangle (disp dc) (drawable dc) (brushGC dc) 0 0 (-1) (-1)  -- entire window (in 2s complement!)
  X.setRegion (disp dc) (brushGC dc) emptyRegion
  return ()
  )

----------------------------------------------------------------
-- End
----------------------------------------------------------------
