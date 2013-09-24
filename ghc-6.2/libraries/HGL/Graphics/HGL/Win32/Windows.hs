module Graphics.HGL.Win32.Windows
	( runGraphics
	, Title
	, Window, mkWindow, openWindow, openWindowEx, closeWindow, redrawWindow
	, RedrawMode(..)
	, getWindowRect
	, Draw, getGraphic, setGraphic, modGraphic, directDraw
	, Event(..), getWindowEvent, maybeGetWindowEvent
	, getWindowTick
	, windowHWND
	) where

import Graphics.HGL.Win32.WND
	( WND, openWND, closeWND, redrawWND, getHWND
	, handleEvents, closeAllHWNDs
	, beginGraphics, endGraphics
	, wndRect, drawWND
	)
import Graphics.HGL.Win32.Types
import Graphics.HGL.Win32.Draw( 
	DrawFun, drawGraphic, drawBufferedGraphic 
	)
import Graphics.HGL.Win32.Event( Event(..) )
import Graphics.HGL.Win32.Events ( Events, newEvents, isNoEvent )
import qualified Graphics.HGL.Win32.Events as Events ( getEvent, getTick )

import Graphics.HGL.Win32.Utilities( safeTry )
import qualified System.Win32 as Win32
import Word( Word32 )

import IOExts
import Concurrent( forkIO, yield )
import IO     ( try )

----------------------------------------------------------------
-- The interface
----------------------------------------------------------------

type Title = String

data Window = MkWindow { 
	events  :: Events,	    -- the event stream
	graphic :: IORef (Draw ())  -- the current graphic
	wnd     :: WND,	    	    -- the real window
	}

data RedrawMode
  = Unbuffered
  | DoubleBuffered

runGraphics  :: IO () -> IO ()
openWindow   :: Title -> Point -> IO Window
openWindowEx :: Title -> Maybe Point -> Point ->
                RedrawMode -> Maybe Word32 -> 
                IO Window
mkWindow     :: WND -> Events -> IORef (Draw ()) -> IO Window
closeWindow  :: Window -> IO ()
redrawWindow :: Window -> IO ()
getWindowRect :: Window -> IO (Point, Point)
getGraphic   :: Window -> IO (Draw ())
setGraphic   :: Window -> Draw () -> IO ()
modGraphic   :: Window -> (Draw () -> Draw ()) -> IO ()
getWindowEvent     :: Window -> IO Event
getWindowTick      :: Window -> IO ()
maybeGetWindowEvent :: Window -> IO (Maybe Event)

-- in case you need low level access
windowHWND   :: Window -> IO Win32.HWND

----------------------------------------------------------------
-- The implementation
----------------------------------------------------------------

-- We took a lot of effort to make sure that we always close the
-- windows - even if "m" fails.
--
-- Note though that we use "try" instead of "safeTry" on the call to
-- "m" because it is quite normal for "m" to block (and safeTry treats
-- blocking as failure).

runGraphics m = do
  beginGraphics
  quit <- newIORef False
  safeTry $ do
    forkIO (try m >> writeIORef quit True)
    yield
    handleEvents (readIORef quit)
  endGraphics

openWindow name size = openWindowEx name Nothing size Unbuffered Nothing
 
openWindowEx name pos size redrawMode tickRate = do
	graphic <- newIORef (return ())
	events  <- newEvents
	let draw = \ hwnd hdc -> do
                      p <- readIORef graphic 
		      repaint p hwnd hdc
	wnd     <- openWND name (fmap fromPoint pos) (Just $ fromPoint size) 
			   events draw tickRate
	mkWindow wnd events graphic
 where
  repaint = case redrawMode of
            Unbuffered     -> drawGraphic
            DoubleBuffered -> drawBufferedGraphic

mkWindow wnd events graphic = do
	return (MkWindow { wnd=wnd, events=events, graphic=graphic })

closeWindow w  = closeWND        (wnd w)
getWindowRect w   = wndRect         (wnd w)
redrawWindow w = redrawWND       (wnd w)
windowHWND w   = getHWND         (wnd w)
getGraphic w   = readIORef       (graphic w)
setGraphic w p = writeIORef      (graphic w) p >> redrawWND (wnd w)
modGraphic w   = modIORef        (graphic w)
directDraw w p = drawWND (wnd w) p
getWindowEvent w     = Events.getEvent (events w)
getWindowTick w      = Events.getTick  (events w)

maybeGetWindowEvent w
  = do noEvent <- isNoEvent(events w)
       if noEvent 
          then return Nothing
          else do ev <- getWindowEvent w
                  return (Just ev)

-- peekEvent :: Window -> IO (Maybe Event)
-- peekEvent w = Events.peekEvent (events w)

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

modIORef :: IORef a -> (a -> a) -> IO ()
modIORef r f = do
  a <- readIORef r
  writeIORef r (f a)

----------------------------------------------------------------
-- The end
----------------------------------------------------------------
