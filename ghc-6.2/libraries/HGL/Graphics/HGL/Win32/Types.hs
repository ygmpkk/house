module Graphics.HGL.Win32.Types
	( Angle
	, Dimension, toDimension, fromDimension
	, Point,     toPoint,     fromPoint
	, Size
	, RGB(RGB), fromRGB, toRGB
	, Bitmap(..)
	, Draw(MkDraw)
        , Graphic
        , unDraw, mkDraw, bracket_, bracket, ioToDraw
	) where

import qualified System.Win32 as Win32
import qualified Graphics.HGL.Win32.Utilities as Utils
import Word( Word8 )  -- GHC extension
import Monad( liftM )

-- Hugs does not allow operators to have different fixities in 
-- different modules (this is a known deviation from Standard Haskell).
-- In consequence, we don't declare any fixities in any non-standard
-- library because it would prevent the programmer from using the same
-- operator name at a different fixity.
--
-- infixr 9 `over`

----------------------------------------------------------------
-- Units
----------------------------------------------------------------

type Angle     = Double

type Dimension = Int
type Point     = (Dimension,Dimension)
type Size      = (Dimension,Dimension)

-- These functions are used when implementing Graphic values
toPoint        :: Win32.POINT -> Point
fromPoint      :: Point -> Win32.POINT

toDimension    :: Win32.INT -> Dimension
fromDimension  :: Dimension -> Win32.INT

toPoint       (x,y) = (toDimension x, toDimension y)
fromPoint     (x,y) = (fromDimension x, fromDimension y)
toDimension   = fromIntegral
fromDimension = fromIntegral

---------------------------------------------------------------
-- Colors
----------------------------------------------------------------

data RGB = RGB Word8 Word8 Word8

fromRGB :: RGB -> Win32.COLORREF
fromRGB (RGB r g b) = Win32.rgb r g b

toRGB :: Win32.COLORREF -> RGB
toRGB c = RGB (Win32.getRValue c) (Win32.getGValue c) (Win32.getBValue c)

----------------------------------------------------------------
-- Bitmaps
----------------------------------------------------------------

newtype Bitmap = MkBitmap Win32.HBITMAP

----------------------------------------------------------------
-- Graphics
----------------------------------------------------------------

type Graphic = Draw ()

newtype Draw a = MkDraw (Win32.HDC -> IO a)
unDraw (MkDraw m) = m
ioToDraw m = MkDraw (\ _ -> m)
mkDraw = MkDraw

-- a standard reader monad
instance Monad Draw where
  return a = MkDraw (\ hdc -> return a)
  m >>= k  = MkDraw (\ hdc -> do { a <- unDraw m hdc; unDraw (k a) hdc })

instance Functor Draw where fmap = liftM

bracket :: Draw a -> (a -> Draw b) -> (a -> Draw c) -> Draw c
bracket left right m = MkDraw (\ hdc ->
  Utils.bracket (unDraw left hdc) 
                (\ a -> unDraw (right a) hdc)
                (\ a -> unDraw (m a) hdc))

bracket_ :: Draw a -> (a -> Draw b) -> Draw c -> Draw c
bracket_ left right m = MkDraw (\ hdc ->
  Utils.bracket_ (unDraw left hdc) 
                 (\ a -> unDraw (right a) hdc)
                 (unDraw m hdc))


----------------------------------------------------------------
