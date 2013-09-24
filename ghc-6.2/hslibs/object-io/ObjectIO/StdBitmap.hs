module StdBitmap where


--	Clean Object I/O library, version 1.2
--	Interface functions for drawing bitmaps.


import	OSBitmap
import	OSPicture
import	CommonDef
import	StdPicture
--import  IO
--import  IOExts(openFileEx, IOModeEx(..))

type Bitmap = OSBitmap

openBitmap :: FilePath -> IO (Maybe Bitmap)
openBitmap name = osReadBitmap name

disposeBitmap = osDisposeBitmap

getBitmapSize :: Bitmap -> Size
getBitmapSize bitmap = fromTuple (osGetBitmapSize bitmap)	

resizeBitmap :: Size -> Bitmap -> Bitmap
resizeBitmap size@(Size {w=w,h=h}) bitmap
	| w<0 || h<0 = error "resizeBitmap" "StdBitmap" "a Size record with negative components was passed"
	| otherwise  = osResizeBitmap (w,h) bitmap

instance Drawables Bitmap where
	draw bitmap = do		
		  penPos <- getPenPos
		  origin <- getPictOrigin
		  osDrawBitmap bitmap (toTuple penPos) (toTuple origin)
	
	drawAt pos bitmap = do
		origin <- getPictOrigin
		osDrawBitmap bitmap (toTuple pos) (toTuple origin)		
	
	undraw bitmap =
		unfill (Box {box_w=w,box_h=h})
		where
			(w,h)	= osGetBitmapSize bitmap
	
	undrawAt pos bitmap =
		unfillAt pos (Box {box_w=w,box_h=h})
		where
			(w,h)	= osGetBitmapSize bitmap
