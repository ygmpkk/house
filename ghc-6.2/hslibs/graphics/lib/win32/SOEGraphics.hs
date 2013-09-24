module SOEGraphics
	( module SOEGraphics
        -- Sigh, Hugs does not implement hiding accurately so the only way
        -- to export everything from GraphicsUtils except "Time" is to
        -- copy the (very long) export list from GraphicsUtils and delete
        -- one line.

        -- GraphicsUtils
        , openWindow        
        , clearWindow       
        , drawInWindow      

        , getWindowSize     
        , getLBP            
        , getRBP            
        , getButton         
        , getKey            
        , getKeyEx          
	, wGetChar

        , emptyGraphic      
        , overGraphic       
        , overGraphics      

        , withFont          
        , withTextColor     
        , withTextAlignment 
        , withBkColor       
        , withBkMode        
        , withPen           
        , withBrush         
        , withRGB           

        , Color(..)
        , colorList  
        , colorTable 
        , withColor  

        , par               
        , par_              
        , parMany           

        -- GraphicsDC
        , Point                      
        , Size                       
        , Angle                      
--        , Time                       
        , RGB                        
        , Alignment                  
        , HAlign(Left',Center,Right')
        , VAlign(Top,Baseline,Bottom)
        , BkMode(Opaque,Transparent)
        , Brush
        , Draw
        , mkBrush             
        , Pen
        , createPen           
        , mkPen               
        , Style(Solid,Dash,Dot,DashDot,DashDotDot,Null,InsideFrame)

        , selectFont          
        , setTextColor        
        , setTextAlignment    
        , setBkColor          
        , setBkMode           
        , selectPen           
        , selectBrush         
                              
        , bracket             
        , bracket_            
                              
        -- GraphicsFont       
        , Font                
        , createFont          
        , deleteFont          
                              
        -- GraphicsWindow     
        , runGraphics         
        , Title               
        , Window
        , RedrawMode(Unbuffered,DoubleBuffered)
        , openWindowEx        
                              
        , closeWindow         
        , getWindowRect       
        , getWindowEvent      
        , getWindowTick  
        , maybeGetWindowEvent 
                              
        , Graphic             
        , setGraphic          
        , getGraphic          
        , modGraphic          
        , directDraw          
        , getTime             

        -- GraphicsPicture
        , arc             
        , ellipse         
        , shearEllipse    
        , line            
        , polyline        
        , polygon         
        , polyBezier -- warning: not in X11
        , text            

        -- GraphicsRegion
        , Region
--        , emptyRegion     
        , rectangleRegion 
        , ellipseRegion   
        , polygonRegion   
        , intersectRegion 
        , unionRegion     
        , subtractRegion  
        , xorRegion    	  
--        , regionToGraphic 

        -- GraphicsEvent
        , Event(..)
	) where

import GraphicsUtils hiding (getKey, getKeyEx, openWindowEx, Event(..), maybeGetWindowEvent)
import qualified GraphicsUtils as HGL

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

openWindowEx        :: Title -> Maybe Point -> Maybe Size -> 
                       RedrawMode -> Maybe Time -> IO Window
createRectangle     :: Point -> Point -> Region
createPolygon       :: [Point] -> Region
createEllipse       :: Point -> Point -> Region
orRegion            :: Region -> Region -> Region
andRegion           :: Region -> Region -> Region
diffRegion          :: Region -> Region -> Region
drawRegion          :: Region -> Graphic
                    
drawGraphic         :: RedrawMode
drawBufferedGraphic :: RedrawMode

drawInWindowNow     :: Window -> Graphic -> IO ()
getEvent            :: Window -> IO HGL.Event

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

openWindowEx a b (Just c) d e = HGL.openWindowEx a b c d e
openWindowEx a b Nothing  d e = ioError (userError err)
 where
  err = "Use of 'Nothing' as the third argument of openWindowEx is no longer supported by the graphics library"

createRectangle = rectangleRegion
createEllipse   = ellipseRegion
createPolygon   = polygonRegion
orRegion        = unionRegion
andRegion       = intersectRegion
diffRegion      = subtractRegion
drawRegion      = regionToGraphic

-- backwards compatability:
drawGraphic           = Unbuffered
drawBufferedGraphic   = DoubleBuffered

-- should have a different way to specify background color
-- drawBufferedGraphicBC :: RGB -> RedrawMode

drawInWindowNow = drawInWindow

getEvent = getWindowEvent

----------------------------------------------------------------
-- Event, getKey, and maybeGetWindowEvent compatibility
----------------------------------------------------------------

{-
 The SOE sources are set in stone, so this module provides the interface
 SOE expects, even if the Graphics library moves on (cf. Event.Key).
-}

-- Wait for a key to go down then a (possibly different) key to go up.
-- Deprecated SOE compatibility.
getKey w = do { getKeyEx w True; getKeyEx w False }

-- wait for key to go down/up
-- Deprecated SOE compatibility.
getKeyEx w down = loop
 where
  loop = do
        e <- getWindowEvent w
        case e of 
          HGL.Key { HGL.keysym = k, HGL.isDown = isDown } 
            |  isDown == down && isCharKey k
            -> return (keyToChar k)
          _ -> loop

-- tiresome, but necessary.
maybeGetWindowEvent :: Window -> IO (Maybe Event)
maybeGetWindowEvent win = do
   mbEv <- HGL.maybeGetWindowEvent win
   return (fmap toSOEEvent mbEv)
 where
   toSOEEvent ev =
     case ev of
       HGL.Char x       -> Key x True
       HGL.Key k isDown -> Key (keyToChar k) isDown
       HGL.Button pt left down -> Button pt left down
       HGL.MouseMove p  -> MouseMove p
       HGL.Resize       -> Resize
       HGL.Closed       -> Closed

data Event 
  = Key       { char :: Char , isDown :: Bool }
  | Button    { pt :: Point, isLeft, isDown :: Bool }
  | MouseMove { pt :: Point }
  | Resize
  | Closed
 deriving Show 

----------------------------------------------------------------
-- End
----------------------------------------------------------------
