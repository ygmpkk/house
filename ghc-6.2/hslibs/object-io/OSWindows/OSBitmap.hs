module OSBitmap where


-- Clean object I/O library, version 1.2
-- PA: other version of bitmaps: create a bitmap handle instead of continuesly copying String to OS

import	OSPicture
import	PictCCall_12
import  Cutil_12
--import  CString(newCString)
--import  MarshalAlloc(free)
--import  IO

data OSBitmap
   = OSBitmap
   	{ originalSize	:: !(Int,Int)		-- The size of the bitmap
	, reSize	:: !(Int,Int)		-- to store values passed to resizeBitmap
	, bitmapHandle	:: !HBITMAP		-- The handle to the screen bitmap (for screen)
	}

osReadBitmap :: FilePath -> IO (Maybe OSBitmap)
osReadBitmap name = do
	ptr <- newCString name
	pWidth <- malloc
	pHeight <- malloc
	hdc <- winCreateScreenHDC
	hbmp <- winCreateBitmap hdc ptr pWidth pHeight
	winDestroyScreenHDC hdc
	w <- fpeek pWidth
	h <- fpeek pHeight	
	free ptr
	(if hbmp == nullPtr then return Nothing
	 else return (Just (OSBitmap {originalSize=(w,h),reSize=(w,h),bitmapHandle=hbmp})))

osDisposeBitmap :: OSBitmap -> IO ()
osDisposeBitmap (OSBitmap {bitmapHandle=handle}) = winDeleteObject handle

-- osGetBitmapSize returns the size of the bitmap.
osGetBitmapSize :: OSBitmap -> (Int,Int)
osGetBitmapSize = reSize

{-	osResizeBitmap (w,h) bitmap
		resizes the argument bitmap to the given size.
	It is assumed that w and h are not negative.
-}
osResizeBitmap :: (Int,Int) -> OSBitmap -> OSBitmap
osResizeBitmap size bitmap = bitmap{reSize=size}

{-	osDrawBitmap bitmap pos origin pictContext
		draws the argument bitmap with the left top corner at pos, given the current origin and drawing context.
-}
osDrawBitmap :: OSBitmap -> (Int,Int) -> (Int,Int) -> Draw ()
osDrawBitmap (OSBitmap {originalSize=originalSize,reSize=reSize,bitmapHandle=bitmapHandle}) pos@(px,py) origin@(ox,oy) =
	Draw (\picture@(Picture {pictToScreen=isScreenOutput,pictContext=context}) ->
		(if originalSize==reSize then
		      winDrawBitmap  originalSize destination bitmapHandle context
		 else
		      winDrawResizedBitmap  originalSize destination reSize bitmapHandle context) >>
		return ((), picture))
	where
		destination	= (px-ox,py-oy)
