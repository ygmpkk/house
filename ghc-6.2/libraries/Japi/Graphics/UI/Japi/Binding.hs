module Graphics.UI.Japi.Binding where

import Graphics.UI.Japi.Constants
import Graphics.UI.Japi.Types

import Foreign
import Foreign.C (CInt(..),CString(..),withCString)
-- import Foreign.C

foreign import ccall "japi.h j_start" j_start :: IO JBool
foreign import ccall "japi.h j_setdebug" j_setdebug :: DebugLevel -> IO ()
foreign import ccall "japi.h j_quit" j_quit :: IO ()
foreign import ccall "japi.h j_kill" j_kill :: IO ()
foreign import ccall "japi.h j_show" j_show :: Object -> IO ()
foreign import ccall "japi.h j_hide" j_hide :: Object -> IO ()
foreign import ccall "japi.h j_pack" j_pack :: Frame -> IO ()
foreign import ccall "japi.h j_setborderlayout" j_setborderlayout :: Frame -> IO ()
foreign import ccall "japi.h j_nextaction" j_nextaction :: IO EventListener
foreign import ccall "japi.h j_getaction" j_getaction :: IO EventListener
foreign import ccall "japi.h j_seticon" j_seticon :: Frame -> Image -> IO ()
foreign import ccall "japi.h j_print" j_print :: Object -> IO ()
foreign import ccall "japi.h j_printer" j_printer :: Frame -> Printer
foreign import ccall "japi.h j_canvas" j_canvas :: Frame -> Int -> Int -> IO Canvas
foreign import ccall "japi.h j_textarea" j_textarea :: Object -> Int -> Int -> IO TextArea
foreign import ccall "japi.h j_textfield" j_textfield :: Object -> Int -> IO TextField
foreign import ccall "japi.h j_setgridlayout" j_setgridlayout :: Frame -> Int -> Int -> IO ()
foreign import ccall "japi.h j_sethgap" j_sethgap :: Frame -> Int -> IO ()
foreign import ccall "japi.h j_setvgap" j_setvgap :: Frame -> Int -> IO ()
foreign import ccall "japi.h j_setpos" j_setpos :: Canvas -> Int -> Int -> IO ()
foreign import ccall "japi.h j_cliprect" j_cliprect :: Canvas -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_drawrect" j_drawrect :: Canvas -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_fillrect" j_fillrect :: Canvas -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_drawroundrect" j_drawroundrect :: Canvas -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_fillroundrect" j_fillroundrect :: Canvas -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_drawoval" j_drawoval :: Canvas -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_filloval" j_filloval :: Canvas -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_drawcircle" j_drawcircle :: Canvas -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_fillcircle" j_fillcircle :: Canvas -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_drawarc" j_drawarc :: Canvas -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_fillarc" j_fillarc :: Canvas -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_drawline" j_drawline :: Canvas -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_drawpixel" j_drawpixel :: Canvas -> Int -> Int -> IO ()
foreign import ccall "japi.h j_mouselistener" j_mouselistener :: Canvas -> EventKind -> IO EventListener
foreign import ccall "japi.h j_setnamedcolor" j_setnamedcolor :: Object -> Colour -> IO ()
foreign import ccall "japi.h j_setnamedcolorbg" j_setnamedcolorbg :: Object -> Colour -> IO ()
foreign import ccall "japi.h j_setcolor" j_setcolor :: Object -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_setcolorbg" j_setcolorbg :: Object -> Int -> Int -> Int -> IO ()
foreign import ccall "japi.h j_setxor" j_setxor :: Canvas -> JBool -> IO ()
foreign import ccall "japi.h j_componentlistener" j_componentlistener :: Object -> EventKind -> IO ComponentListener
foreign import ccall "japi.h j_borderpanel" j_borderpanel :: Frame -> ComponentKind -> IO Panel
foreign import ccall "japi.h j_setflowlayout" j_setflowlayout :: Panel -> Orientation -> IO ()
foreign import ccall "japi.h j_dispose" j_dispose :: Alert -> IO ()
foreign import ccall "japi.h j_sleep" j_sleep :: Int -> IO ()
foreign import ccall "japi.h j_sync" j_sync :: IO ()
foreign import ccall "japi.h j_beep" j_beep :: IO ()
foreign import ccall "japi.h j_random" j_random :: IO Int
foreign import ccall "japi.h j_isresizable" j_isresizable :: Object -> IO JBool
foreign import ccall "japi.h j_isvisible" j_isvisible :: Object -> IO JBool
foreign import ccall "japi.h j_isparent" j_isparent :: Object -> Object -> IO JBool
foreign import ccall "japi.h j_isselect" j_isselect :: Object -> Object -> IO JBool
foreign import ccall "japi.h j_getrows" j_getrows :: Object -> IO Int
foreign import ccall "japi.h j_getcolumns" j_getcolumns :: Object -> IO Int
foreign import ccall "japi.h j_getstate" j_getstate :: Object -> IO JBool
foreign import ccall "japi.h j_getselect" j_getselect :: Object -> IO Int
foreign import ccall "japi.h j_getselend" j_getselend :: Object -> IO Int
foreign import ccall "japi.h j_getselstart" j_getselstart :: Object -> IO Int
foreign import ccall "japi.h j_getvalue" j_getvalue :: Object -> IO Int
foreign import ccall "japi.h j_getscreenheight" j_getscreenheight :: IO Int
foreign import ccall "japi.h j_getscreenwidth" j_getscreenwidth :: IO Int
foreign import ccall "japi.h j_getviewportwidth" j_getviewportwidth :: Object -> IO Int
foreign import ccall "japi.h j_getviewportheight" j_getviewportheight :: Object -> IO Int
foreign import ccall "japi.h j_getxpos" j_getxpos :: Object -> IO Int
foreign import ccall "japi.h j_getypos" j_getypos :: Object -> IO Int
foreign import ccall "japi.h j_getmousex" j_getmousex :: EventListener -> IO Int
foreign import ccall "japi.h j_getmousey" j_getmousey :: EventListener -> IO Int
foreign import ccall "japi.h j_getwidth" j_getwidth :: Canvas -> IO Int
foreign import ccall "japi.h j_getheight" j_getheight :: Canvas -> IO Int

foreign import ccall "japi.h j_getmousepos" c_j_getmousepos :: EventListener -> Ptr Int -> Ptr Int -> IO ()
j_getmousepos :: EventListener -> IO (Int, Int)
j_getmousepos listener = alloca $ \px -> 
			 alloca $ \py ->
			     do c_j_getmousepos listener px py
				x <- peek px
				y <- peek py
				return (x, y)

foreign import ccall "japi.h j_getpos" c_j_getpos :: EventListener -> Ptr Int -> Ptr Int -> IO ()
j_getpos :: EventListener -> IO (Int, Int)
j_getpos listener = alloca $ \px -> 
			 alloca $ \py -> do c_j_getpos listener px py
					    x <- peek px
					    y <- peek py
					    return (x, y)

foreign import ccall "japi.h j_frame" c_j_frame :: CString -> IO Frame
j_frame :: String -> IO Frame
j_frame name = withCString name $ \cname -> c_j_frame cname

foreign import ccall "japi.h j_loadimage" c_j_loadimage :: CString -> IO Image
j_loadimage :: String -> IO Image
j_loadimage name = withCString name $ \cname -> c_j_loadimage cname

foreign import ccall "japi.h j_appendtext" c_j_appendtext :: Object -> CString -> IO ()
j_appendtext :: Object -> String -> IO ()
j_appendtext obj name = withCString name $ \cname -> c_j_appendtext obj cname

foreign import ccall "japi.h j_settext" c_j_settext :: Object -> CString -> IO ()
j_settext :: Object -> String -> IO ()
j_settext obj name = withCString name $ \cname -> c_j_settext obj cname

foreign import ccall "japi.h j_label" c_j_label :: Panel -> CString -> IO Label
j_label :: Panel -> String -> IO Label
j_label panel name = withCString name $ \cname -> c_j_label panel cname

foreign import ccall "japi.h j_messagebox" c_j_messagebox :: Frame -> CString -> CString -> IO Alert
j_messagebox :: Frame -> String -> String -> IO Alert
j_messagebox frame name name1 = withCString name $ \cname -> 
				withCString name1 $ \cname1 -> c_j_messagebox frame cname cname1

foreign import ccall "japi.h j_alertbox" c_j_alertbox :: Frame -> CString -> CString -> CString -> IO Alert
j_alertbox :: Frame -> String -> String -> String -> IO Alert
j_alertbox frame name name1 name2 = withCString name $ \cname -> 
				    withCString name1 $ \cname1 ->
				    withCString name2 $ \cname2 -> c_j_alertbox frame cname cname1 cname2

foreign import ccall "japi.h j_choicebox2" c_j_choicebox2 :: Frame -> CString -> CString
							  -> CString -> CString -> IO Alert
j_choicebox2 :: Frame -> String -> String -> String -> String -> IO Alert
j_choicebox2 frame name name1 name2 name3 = withCString name $ \cname -> 
					    withCString name1 $ \cname1 ->
					    withCString name2 $ \cname2 ->
				            withCString name3 $ \cname3 ->
						c_j_choicebox2 frame cname cname1 cname2 cname3

foreign import ccall "japi.h j_choicebox3" c_j_choicebox3 :: Frame -> CString -> CString
							  -> CString -> CString -> CString -> IO Alert
j_choicebox3 :: Frame -> String -> String -> String -> String -> String -> IO Alert
j_choicebox3 frame name name1 name2 name3 name4 = withCString name $ \cname -> 
				                  withCString name1 $ \cname1 ->
				                  withCString name2 $ \cname2 ->
				                  withCString name3 $ \cname3 ->
				                  withCString name4 $ \cname4 ->
						      c_j_choicebox3 frame cname cname1 cname2 cname3 cname4

foreign import ccall "japi.h j_menubar" j_menubar :: Frame -> IO MenuBar

foreign import ccall "japi.h j_menu" c_j_menu :: MenuBar -> CString -> IO Menu
j_menu :: MenuBar -> String -> IO Menu
j_menu menubar label = withCString label $ \clabel -> c_j_menu menubar clabel

foreign import ccall "japi.h j_menuitem" c_j_menuitem :: Menu -> CString -> IO MenuItem
j_menuitem :: Menu -> String -> IO MenuItem
j_menuitem menu label = withCString label $ \clabel -> c_j_menuitem menu clabel

foreign import ccall "japi.h j_drawpolyline" c_j_drawpolyline :: Canvas -> Int -> Ptr Int -> Ptr Int -> IO ()
j_drawpolyline :: Canvas -> [Int] -> [Int] -> IO ()
j_drawpolyline canvas xs ys  = if lys /= lxs
				   then error "j_drawpolyline has ill-matched input list lengths"
	 			   else withArray xs $ \pxs -> 
			 	        withArray ys $ \pys ->
					    c_j_drawpolyline canvas (fromIntegral $ lxs) pxs pys
	where lxs = length xs
	      lys = length ys

foreign import ccall "japi.h j_drawpolygon" c_j_drawpolygon :: Canvas -> Int -> Ptr Int -> Ptr Int -> IO ()
j_drawpolygon :: Canvas -> [Int] -> [Int] -> IO ()
j_drawpolygon canvas xs ys  = if lys /= lxs
				   then error "j_drawpolygon has ill-matched input list lengths"
	 			   else withArray xs $ \pxs -> 
			 	        withArray ys $ \pys ->
					    c_j_drawpolygon canvas (fromIntegral $ lxs) pxs pys
	where lxs = length xs
	      lys = length ys

foreign import ccall "japi.h j_fillpolygon" c_j_fillpolygon :: Canvas -> Int -> Ptr Int -> Ptr Int -> IO ()
j_fillpolygon :: Canvas -> [Int] -> [Int] -> IO ()
j_fillpolygon canvas xs ys  = if lys /= lxs
				   then error "j_fillpolygon has ill-matched input list lengths"
	 			   else withArray xs $ \pxs -> 
			 	        withArray ys $ \pys ->
					    c_j_fillpolygon canvas (fromIntegral $ lxs) pxs pys
	where lxs = length xs
	      lys = length ys

foreign import ccall "japi.h j_drawstring" c_j_drawstring :: Canvas -> Int -> Int -> CString -> IO ()
j_drawstring :: Canvas -> Int -> Int -> String -> IO ()
j_drawstring canvas x y str = withCString str $ \cstr ->  c_j_drawstring canvas x y cstr

foreign import ccall "japi.h j_drawimagesource" c_j_drawimagesource :: Canvas -> Int -> Int -> Int -> Int
								    -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
j_drawimagesource :: Canvas -> Int -> Int -> Int -> Int
		  -> [Int] -> [Int] -> [Int] -> IO ()
j_drawimagesource canvas x y w h rs gs bs =
	 if lrs /= lgs || lrs /= lbs
	   then error "j_drawimagesource has ill-matched input list lengths"
	   else if wh /= (fromIntegral $ lrs)
		    then  error "j_drawimagesource has ill-matched width, height and data size"
		    else withArray rs $ \prs -> 
			 withArray gs $ \pgs ->
			 withArray bs $ \pbs ->
			     c_j_drawimagesource canvas x y w h prs pgs pbs
	where lrs = length rs
	      lgs = length gs
	      lbs = length bs
	      wh  = w * h

