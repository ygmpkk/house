module Graphics.HGL.Win32.Region
	( Region
	, intersectRegion, unionRegion, xorRegion, subtractRegion
	, rectangleRegion, ellipseRegion, polygonRegion
	, regionToGraphic
	) where

import qualified System.Win32 as Win32
import Graphics.HGL.Win32.Types hiding (bracket)
import Graphics.HGL.Win32.Utilities( bracket )
import IOExts(unsafePerformIO)

----------------------------------------------------------------
-- The Interface
----------------------------------------------------------------

newtype Region = MkRegion Win32.HRGN

intersectRegion :: Region -> Region -> Region
unionRegion     :: Region -> Region -> Region
xorRegion       :: Region -> Region -> Region
subtractRegion  :: Region -> Region -> Region
	        
rectangleRegion :: Point -> Point -> Region
ellipseRegion   :: Point -> Point -> Region
polygonRegion   :: [Point] -> Region

regionToGraphic :: Region -> Draw ()

----------------------------------------------------------------
-- The Implementation
----------------------------------------------------------------

rectangleRegion pt0 pt1 = unsafePerformIO $ do
  r <- Win32.createRectRgn x0 y0 x1 y1
  return (MkRegion r)
 where
  (x0,y0) = fromPoint pt0
  (x1,y1) = fromPoint pt1

-- Sigh! createEllipticRgn raises an exception if either dimension
-- of the ellipse is empty.  We hack around this by using rectangleRegion
-- in the problematic case (since createRectRgn behaves sensibly).
ellipseRegion pt0 pt1 
  | x0 /= x1 && y0 /= y1
  = unsafePerformIO $ do
      r <- Win32.createEllipticRgn x0 y0 x1 y1
      return (MkRegion r)
  | otherwise
  = rectangleRegion pt0 pt1
 where
  (x0,y0) = fromPoint pt0
  (x1,y1) = fromPoint pt1

polygonRegion pts = unsafePerformIO $ do
  r <- Win32.createPolygonRgn (map fromPoint pts) Win32.wINDING
  return (MkRegion r)

-- combine :: Win32.ClippingMode -> Region -> Region -> Region -> IO ()
-- combine mode (MkRegion r1) (MkRegion r2) (MkRegion result) = do
--   Win32.combineRgn result r1 r2 mode
--   return ()

combine :: Win32.ClippingMode -> Region -> Region -> Region
combine mode (MkRegion r1) (MkRegion r2) = unsafePerformIO $ do
  r <- Win32.createRectRgn 0 0 0 0
  Win32.combineRgn r r1 r2 mode
  return (MkRegion r)

regionToGraphic (MkRegion r) = mkDraw (\hdc -> Win32.paintRgn hdc r)

intersectRegion = combine Win32.rGN_AND
unionRegion     = combine Win32.rGN_OR
xorRegion       = combine Win32.rGN_XOR
subtractRegion  = combine Win32.rGN_DIFF

----------------------------------------------------------------
-- End
----------------------------------------------------------------

