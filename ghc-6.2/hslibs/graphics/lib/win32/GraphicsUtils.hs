module GraphicsUtils
        ( module GraphicsCore
        , module GraphicsUtils
        ) where

import GraphicsCore
import Concurrent
        ( MVar, newEmptyMVar, newMVar, takeMVar, putMVar
        , forkIO
        )
import Ix(Ix)
import Array(Array,array,(!))

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

openWindow        :: Title -> Size -> IO Window
clearWindow       :: Window -> IO ()
drawInWindow      :: Window -> Graphic -> IO ()

getWindowSize     :: Window -> IO Size
getLBP            :: Window -> IO Point
getRBP            :: Window -> IO Point
getButton         :: Window -> Bool -> Bool -> IO Point
getKey            :: Window -> IO Key
getKeyEx          :: Window -> Bool -> IO Key
wGetChar          :: Window -> IO Char

emptyGraphic      :: Graphic
overGraphic       :: Graphic -> Graphic -> Graphic
overGraphics      :: [Graphic] -> Graphic

withFont          :: Font      -> Graphic -> Graphic
withTextColor     :: RGB       -> Graphic -> Graphic
withTextAlignment :: Alignment -> Graphic -> Graphic
withBkColor       :: RGB       -> Graphic -> Graphic
withBkMode        :: BkMode    -> Graphic -> Graphic
withPen           :: Pen       -> Graphic -> Graphic
withBrush         :: Brush     -> Graphic -> Graphic
withRGB           :: RGB       -> Graphic -> Graphic

data Color 
  = Black
  | Blue
  | Green 
  | Cyan
  | Red 
  | Magenta
  | Yellow
  | White
 deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

colorList  :: [(Color, RGB)]
colorTable :: Array Color RGB
withColor  :: Color -> Graphic -> Graphic

par               :: IO a -> IO b -> IO (a,b)
par_              :: IO a -> IO b -> IO ()
parMany           :: [IO ()] -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

-- Window operations

openWindow name size = openWindowEx name Nothing size Unbuffered Nothing

clearWindow w = setGraphic w emptyGraphic

getWindowSize w = do
        (pt,sz) <- getWindowRect w
        return sz

drawInWindow w p = do
        modGraphic w (p `overGraphic`)
        directDraw w p

-- Event operations

-- wait for left/right mouse button up (SOE p148)
getLBP w = getButton w True  True
getRBP w = getButton w False True

-- wait for a key to go down then a (possibly different) key to go up
getKey w = do { getKeyEx w True; getKeyEx w False }

-- wait for key to go down/up
getKeyEx w down = loop
 where
  loop = do
        e <- getWindowEvent w
        case e of 
          Key { keysym = k, isDown = isDown } 
            |  isDown == down 
            -> return k
          _ -> loop

getButton w left down = loop
 where
  loop = do
        e <- getWindowEvent w
        case e of 
          Button {pt=pt,isLeft=isLeft,isDown=isDown} 
            | isLeft == left && isDown == down
            -> return pt
          _ -> loop

-- Wait for a translated character. Use in preference to getKey if the
-- aim is to read text.
wGetChar w = loop
 where
  loop = do
        e <- getWindowEvent w
        case e of 
          Char {char = c} -> return c
          _               -> loop

-- Graphic

--elsewhere: type Graphic = Draw ()
emptyGraphic        = return ()
g1 `overGraphic` g2 = g2 >> g1
overGraphics        = foldr overGraphic emptyGraphic

-- Graphic modifiers

withFont          x = bracket_ (selectFont       x) selectFont
withTextAlignment x = bracket_ (setTextAlignment x) setTextAlignment
withTextColor     x = bracket_ (setTextColor     x) setTextColor
withBkColor       x = bracket_ (setBkColor       x) setBkColor
withBkMode        x = bracket_ (setBkMode        x) setBkMode
withPen           x = bracket_ (selectPen        x) selectPen
withBrush         x = bracket_ (selectBrush      x) selectBrush

withRGB c p = 
  mkBrush c       $ \ brush ->
  withBrush brush $
  mkPen Solid 2 c $ \ pen ->
  withPen pen     $
  withTextColor c $
  p

colorList =
  [ (Black   , RGB   0   0   0)
  , (Blue    , RGB   0   0 255)
  , (Green   , RGB   0 255   0)
  , (Cyan    , RGB   0 255 255)
  , (Red     , RGB 255   0   0)
  , (Magenta , RGB 255   0 255)
  , (Yellow  , RGB 255 255   0)
  , (White   , RGB 255 255 255)
  ]

colorTable = array (minBound, maxBound) colorList

withColor c g = withRGB (colorTable ! c) g 

-- Concurrency primitives

par m1 m2 = do
  v1 <- newEmptyMVar 
  v2 <- newEmptyMVar 
  forkIO (m1 >>= putMVar v1)
  forkIO (m2 >>= putMVar v2)
  a <- takeMVar v1
  b <- takeMVar v2
  return (a,b)

par_ m1 m2 = do
  v1 <- newEmptyMVar 
  v2 <- newEmptyMVar 
  forkIO (m1 >> putMVar v1 ())
  forkIO (m2 >> putMVar v2 ())
  takeMVar v1
  takeMVar v2
  return ()

parMany ms = foldr par_ (return ()) ms

----------------------------------------------------------------
-- End
----------------------------------------------------------------
