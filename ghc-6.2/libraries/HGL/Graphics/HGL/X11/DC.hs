-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.X11.DC
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

module Graphics.HGL.X11.DC
	( DC(..)
	, DC_Bits(..)
        , openDisplay, closeDisplay, getDisplay
	, setTextColor, setBkColor, setBkMode, setTextAlignment
	, selectBrush, selectPen, selectFont
	, Draw, unDraw, ioToDraw, mkDraw, bracket, bracket_
	, drawUnbuffered, drawBuffered, erase
	, Font
	, Pen, createPen, mkPen, defaultPen, lookupColor
	, Brush, mkBrush
	, Style(..)
	, BkMode(..)
	, Alignment, HAlign(..), VAlign(..)
	, RGB(..)
	, Point, fromPoint, toPoint
	, Size,  fromSize,  toSize
	, Angle
	, minAndDelta, avg
	) where

import qualified Graphics.X11.Xlib as X
import Foreign( Word8 )
import Data.Bits
import System.IO.Unsafe( unsafePerformIO )
import Data.IORef( IORef, readIORef, writeIORef )
import Control.Concurrent( MVar, newMVar, takeMVar, putMVar, readMVar )
import Monad( liftM, when )
import Ix( Ix )
import Maybe( isJust )
import qualified Graphics.HGL.X11.Utilities as Utils
import Graphics.HGL.X11.Utilities( modMVar )

----------------------------------------------------------------
-- Device Context (simulates Win32 Device Contexts)
----------------------------------------------------------------

data DC = MkDC
  { disp     :: X.Display 
  , drawable :: X.Drawable
  , textGC   :: X.GC
  , paintGC  :: X.GC
  , brushGC  :: X.GC
  , ref_rect :: MVar ((X.Position,X.Position),(X.Dimension, X.Dimension)) 
  , ref_bits :: MVar DC_Bits
  }

data DC_Bits = DC_Bits
  { textColor  	  :: RGB
  , bkColor    	  :: RGB
  , bkMode     	  :: BkMode
  , textAlignment :: Alignment
  , brush         :: Brush
  , pen           :: Pen
  , font          :: Font
  }

mkBrush           :: RGB                 -> (Brush -> Draw a) -> Draw a
mkPen             :: Style -> Int -> RGB -> (Pen   -> Draw a) -> Draw a
createPen         :: Style -> Int -> RGB -> IO Pen

selectFont       :: Font          -> Draw Font  
setTextColor     :: RGB           -> Draw RGB
setTextAlignment :: Alignment     -> Draw Alignment
setBkColor       :: RGB           -> Draw RGB
setBkMode        :: BkMode        -> Draw BkMode
selectPen        :: Pen           -> Draw Pen  
selectBrush      :: Brush         -> Draw Brush

data RGB = RGB Word8 Word8 Word8

data BkMode = Opaque | Transparent

type Alignment = (HAlign, VAlign)

-- names have a tick to distinguish them from Prelude names (blech!)
data HAlign = Left' | Center   | Right'
 deriving (Enum, Eq, Ord, Ix, Show)
data VAlign = Top   | Baseline | Bottom
 deriving (Enum, Eq, Ord, Ix, Show)

type Font = X.FontStruct
type Brush = RGB

type Pen = (Style,Int,X.Pixel)
data Style
  = Solid 
  | Dash	  -- "-------"
  | Dot		  -- "......."	
  | DashDot	  -- "_._._._"	
  | DashDotDot	  -- "_.._.._"	
  | Null
  | InsideFrame

bracket  :: Draw a -> (a -> Draw b) -> (a -> Draw c) -> Draw c
bracket_ :: Draw a -> (a -> Draw b) -> Draw c -> Draw c

openDisplay  :: String -> IO () -> IO X.Display
closeDisplay :: IO ()
getDisplay   :: IO X.Display

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

displayRef :: MVar (Maybe X.Display)
displayRef = unsafePerformIO (newMVar Nothing)

openDisplay host cleanup = do
  mb_display <- readMVar displayRef
  when (isJust mb_display) cleanup
  openDisplay'
 where
  openDisplay' = do      
    display <- X.openDisplay host `catch` \ err -> 
                 ioError (userError ("Unable to open X display " ++ host))
    modMVar displayRef (const $ Just display)
    return display

closeDisplay = do
  mb_display <- takeMVar displayRef
  case mb_display of
    Nothing      -> do
      putMVar displayRef Nothing
    Just display -> do
      X.closeDisplay display
      putMVar displayRef Nothing

getDisplay = do
  mb_display <- readMVar displayRef
  case mb_display of
    Nothing      -> ioError $ userError "Display not opened yet"
    Just display -> return display

----------------------------------------------------------------
-- Brush
--
-- Used to fill shapes
----------------------------------------------------------------

mkBrush c g = g c

----------------------------------------------------------------
-- Pens
--
-- Used to draw lines and boundaries of filled shapes
----------------------------------------------------------------

defaultPen :: X.Pixel -> Pen
defaultPen col = (Solid,0,col)

createPen style width col = do
  display <- getDisplay
  pixel <- lookupColor display col
  return (style,width,pixel)

mkPen style width color g = do
  p <- ioToDraw $ createPen style width color
  g p

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

setTextColor     x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{textColor=x}
  p <- lookupColor (disp dc) x
  X.setForeground (disp dc) (textGC dc) p
  return (textColor bs)
setBkColor       x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{bkColor=x}
  p <- lookupColor (disp dc) x
  X.setBackground (disp dc) (textGC dc) p
  return (bkColor bs)
setBkMode        x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{bkMode=x}
  return (bkMode bs)
setTextAlignment x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{textAlignment=x}
  return (textAlignment bs)
-- ToDo: how do I set background colour for brush and pen?
selectBrush      x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{brush=x}
  p <- lookupColor (disp dc) x
  X.setForeground (disp dc) (brushGC dc) p
  return (brush bs)
selectPen p@(_,lwidth,c) = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{pen=p}
  X.setForeground (disp dc) (paintGC dc) c
  X.setLineAttributes (disp dc) (paintGC dc) lwidth X.lineSolid X.capButt X.joinMiter
  return (pen bs)
selectFont       x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{font=x}
  X.setFont (disp dc) (textGC dc) (X.fontFromFontStruct x)
  return (font bs)

----------------------------------------------------------------
-- Draw
----------------------------------------------------------------

newtype Draw a = MkDraw (DC -> IO a)

unDraw :: Draw a -> (DC -> IO a)
unDraw (MkDraw m) = m

ioToDraw :: IO a -> Draw a
ioToDraw m = MkDraw (\ _ -> m)

mkDraw :: (DC -> IO a) -> Draw a
mkDraw = MkDraw

-- a standard reader monad
instance Monad Draw where
  return a = MkDraw (\ dc -> return a)
  m >>= k  = MkDraw (\ dc -> do { a <- unDraw m dc; unDraw (k a) dc })
  m >>  k  = MkDraw (\ dc -> do { unDraw m dc; unDraw k dc })

instance Functor Draw where fmap = liftM

bracket left right m = MkDraw (\ dc ->
  Utils.bracket (unDraw left dc) 
                (\ a -> unDraw (right a) dc)
                (\ a -> unDraw (m a) dc))

bracket_ left right m = MkDraw (\ dc ->
  Utils.bracket_ (unDraw left dc) 
                 (\ a -> unDraw (right a) dc)
                 (unDraw m dc))

drawUnbuffered :: DC -> Draw () -> IO ()
drawUnbuffered dc p = do
    unDraw erase dc
    unDraw p dc

drawBuffered :: DC -> Draw () -> X.GC -> Int -> IORef (Maybe X.Pixmap) -> IO ()
drawBuffered dc p gc depth ref_mbuffer = do
    (_,(width,height)) <- readMVar (ref_rect dc)
    -- Note: The buffer is deallocated whenever the window size changes!
    mbuffer <- readIORef ref_mbuffer
    buffer <- case mbuffer of
                  Nothing -> X.createPixmap (disp dc)
		                            (drawable dc)
					    width
					    height
					    depth
		  Just buffer -> return buffer
    X.fillRectangle (disp dc) buffer gc 0 0 width height
    unDraw p dc{drawable=buffer}
    X.copyArea (disp dc) buffer (drawable dc) (paintGC dc) 0 0 width height 0 0
    writeIORef ref_mbuffer (Just buffer)

lookupColor :: X.Display -> RGB -> IO X.Pixel
lookupColor display col = (do
  (p,_,_,_,_) <- X.allocColor display color_map (0,r,g,b,xcolor_flags)
  return p)
     `catch` \ err -> 
               print err >> return 0
--	       ioError (userError ("Error: " ++ show err
--			      ++ "\nUnable to allocate colo[u]r " ++ show (r,g,b) 
--			      ++ " - I'll bet you're running Netscape."))
 where
  screen    = X.defaultScreenOfDisplay display
  color_map = X.defaultColormapOfScreen screen

  RGB r' g' b' = col
  (r,g,b) = ((fromIntegral r') * 256, (fromIntegral g') * 256, (fromIntegral b')*256)

xcolor_flags :: Word8
xcolor_flags = X.doRed .|. X.doGreen .|. X.doBlue

erase	:: Draw ()
erase	= mkDraw (\ dc -> X.clearWindow (disp dc) (drawable dc))

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type Point = (Int,Int)
type Size  = (Int,Int) -- SOE, p47
type Angle = Double

fromPoint :: Point -> X.Point
toPoint   :: X.Point -> Point
fromSize  :: Size -> (X.Dimension, X.Dimension)
toSize    :: (X.Dimension, X.Dimension) -> Size

fromPoint (x,y) = (fromIntegral x, fromIntegral y)
toPoint   (x,y) = (fromIntegral x, fromIntegral y)
fromSize  (x,y) = (fromIntegral x, fromIntegral y)
toSize    (x,y) = (fromIntegral x, fromIntegral y)

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

-- delta is always +ve
minAndDelta :: Int -> Int -> (Int,Int)
minAndDelta a b 
  | a <= b    = (a, b-a)
  | otherwise = (b, a-b)

-- avg :: Int32 -> Int32 -> Int32
avg a b = (a + b) `div` 2

----------------------------------------------------------------
-- End
----------------------------------------------------------------
