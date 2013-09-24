module Graphics.HGL.Win32.Brush
	( Brush
	, createBrush, deleteBrush, selectBrush
	-- , blackBrush, whiteBrush
	) where

import Graphics.HGL.Win32.Types
import qualified System.Win32 as Win32

----------------------------------------------------------------
-- The interface
----------------------------------------------------------------

newtype Brush = MkBrush Win32.HBRUSH

createBrush :: RGB   -> IO Brush
deleteBrush :: Brush -> IO ()
selectBrush :: Brush -> Draw Brush

----------------------------------------------------------------
-- The implementation
----------------------------------------------------------------

createBrush (RGB r g b) = Win32.createSolidBrush (Win32.rgb r g b) >>= return . MkBrush
deleteBrush (MkBrush b) = Win32.deleteBrush b
selectBrush (MkBrush b) = mkDraw (\hdc -> do
  b' <- Win32.selectBrush hdc b
  return (MkBrush b'))

----------------------------------------------------------------
-- 
-- -- special cases - these should _never_ be deleted
-- blackBrush :: IO Brush
-- whiteBrush :: IO Brush
-- 
-- blackBrush = Win32.getStockBrush Win32.bLACK_BRUSH >>= return . MkBrush
-- whiteBrush = Win32.getStockBrush Win32.wHITE_BRUSH >>= return . MkBrush
-- 
----------------------------------------------------------------
