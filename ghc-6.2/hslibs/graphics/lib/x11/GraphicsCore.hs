module GraphicsCore
        ( 
        -- GraphicsDC
          Point                       -- = (Int,Int)
        , Size                        -- = (Int,Int)
        , Angle                       -- = Double
        , Time                        -- = Word32
        , RGB(..)                     -- = RGB Word8 Word8 Word8
        , Alignment                   -- = (HAlign, VAlign)
        , HAlign(Left',Center,Right') -- deriving (Enum, Eq, Ord, Ix, Show)
        , VAlign(Top,Baseline,Bottom) -- deriving (Enum, Eq, Ord, Ix, Show)
        , BkMode(Opaque,Transparent)
        , Brush
        , Draw
        , ioToDraw
        , mkBrush             -- :: RGB                 -> (Brush -> Draw a) -> Draw a
        , Pen
        , createPen           -- :: Style -> Int -> RGB -> IO Pen
        , mkPen               -- :: Style -> Int -> RGB -> (Pen   -> Draw a) -> Draw a
        , Style(Solid,Dash,Dot,DashDot,DashDotDot,Null,InsideFrame)

        , selectFont          -- :: Font          -> Draw Font  
        , setTextColor        -- :: RGB           -> Draw RGB
        , setTextAlignment    -- :: Alignment     -> Draw Alignment
        , setBkColor          -- :: RGB           -> Draw RGB
        , setBkMode           -- :: BkMode        -> Draw BkMode
        , selectPen           -- :: Pen           -> Draw Pen  
        , selectBrush         -- :: Brush         -> Draw Brush
                              
        , bracket             -- :: Draw a -> (a -> Draw b) -> (a -> Draw c) -> Draw c
        , bracket_            -- :: Draw a -> (a -> Draw b) -> Draw c -> Draw c
                              
        -- GraphicsFont       
        , Font                
        , createFont          -- :: Point -> Angle -> Bool -> Bool -> String -> IO Font
        , deleteFont          -- :: Font -> IO ()

        -- GraphicsWindow
        , runGraphics         -- :: IO () -> IO ()
        , Title               -- = String
        , Window
        , RedrawMode(Unbuffered,DoubleBuffered)
        , openWindowEx        -- :: Title -> Maybe Point -> Maybe Size -> 
                              --    RedrawMode -> Maybe Time -> IO Window
        , closeWindow         -- :: Window -> IO ()
        , getWindowRect       -- :: Window -> IO (Point,Point)
        , getWindowEvent      -- :: Window -> IO Event
        , getWindowTick       -- :: Window -> IO ()
        , maybeGetWindowEvent -- :: Window -> IO (Maybe Event)

        , Graphic             -- should be defined elsewhere
        , setGraphic          -- :: Window -> Graphic -> IO ()
        , getGraphic          -- :: Window -> IO Graphic
        , modGraphic          -- :: Window -> (Graphic -> Graphic) -> IO ()
        , directDraw          -- :: Window -> Graphic -> IO ()
        , getTime             -- :: IO Time

        -- GraphicsPicture
        , arc             -- :: Point -> Point -> Angle -> Angle -> Graphic
        , ellipse         -- :: Point -> Point           	 -> Graphic
        , shearEllipse    -- :: Point -> Point -> Point  	 -> Graphic
        , line            -- :: Point -> Point           	 -> Graphic
        , polyline        -- :: [Point]                          -> Graphic 
        , polygon         -- :: [Point]                          -> Graphic 
        -- not in X11: polyBezier   :: [Point] -> Graphic
        , text            -- :: Point -> String                  -> Graphic

        -- GraphicsRegion
        , Region
        , emptyRegion     -- :: Region
        , rectangleRegion -- :: Point -> Point -> Region
        , ellipseRegion   -- :: Point -> Point -> Region
        , polygonRegion   -- :: [Point] -> Region
        , intersectRegion -- :: Region -> Region -> Region
        , unionRegion     -- :: Region -> Region -> Region
        , subtractRegion  -- :: Region -> Region -> Region
        , xorRegion    	  -- :: Region -> Region -> Region
        , regionToGraphic -- :: Region -> Graphic

        -- GraphicsEvent
        , Event(Char,Key,Button,MouseMove,Resize,Closed) -- deriving(Show)
        , char            -- :: Event -> Char
	, keysym          -- :: Event -> Key
        , isDown          -- :: Event -> Bool
        , pt              -- :: Event -> Point
        , isLeft          -- :: Event -> Bool
	
        -- GraphicsKey
        , Key		  -- Abstract!
        , keyToChar	  -- :: Key -> Char 
        , isCharKey	  -- :: Key -> Bool
        , isBackSpaceKey  -- :: Key -> Bool
        , isTabKey	  -- :: Key -> Bool
--        , isLineFeedKey   -- :: Key -> Bool
        , isClearKey 	  -- :: Key -> Bool
        , isReturnKey 	  -- :: Key -> Bool
        , isEscapeKey	  -- :: Key -> Bool
        , isDeleteKey 	  -- :: Key -> Bool
--        , isMultiKeyKey   -- :: Key -> Bool
        , isHomeKey	  -- :: Key -> Bool
        , isLeftKey	  -- :: Key -> Bool
        , isUpKey	  -- :: Key -> Bool
        , isRightKey	  -- :: Key -> Bool
        , isDownKey	  -- :: Key -> Bool
        , isPriorKey	  -- :: Key -> Bool
        , isPageUpKey	  -- :: Key -> Bool
        , isNextKey	  -- :: Key -> Bool
        , isPageDownKey	  -- :: Key -> Bool
        , isEndKey	  -- :: Key -> Bool
--        , isBeginKey	  -- :: Key -> Bool
        , isShiftLKey	  -- :: Key -> Bool
        , isShiftRKey	  -- :: Key -> Bool
        , isControlLKey	  -- :: Key -> Bool
        , isControlRKey	  -- :: Key -> Bool
--        , isCapsLockKey	  -- :: Key -> Bool
--        , isShiftLockKey  -- :: Key -> Bool
--        , isMetaLKey	  -- :: Key -> Bool
--        , isMetaRKey	  -- :: Key -> Bool
--        , isAltLKey	  -- :: Key -> Bool
--        , isAltRKey	  -- :: Key -> Bool
        ) where

import GraphicsEvent
import GraphicsWindow
import GraphicsPicture
import GraphicsRegion
import GraphicsDC
import GraphicsTimer
import GraphicsFont
import GraphicsKey

