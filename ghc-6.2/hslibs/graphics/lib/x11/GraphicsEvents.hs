module GraphicsEvents(
	Events, newEvents, getEvent, sendEvent, isNoEvent,
        getTick, sendTick,
	Event
	) where

import GraphicsEvent( Event )
import GraphicsFlag
import Concurrent(Chan, newChan, readChan, writeChan, isEmptyChan)

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- Events are more or less just a channel (~list) of events
--
-- The only subtlety is that ticks are not part of the channel:
-- they're a separate "flag" so that ticks don't accumulate in the 
-- queue (if you process them too fast) and so that ticks can 
-- "overtake" other events.
-- (Win32 timers do the same thing.  I was rather surprised to find
-- myself reimplementing this in Haskell (even in the Win32 version
-- of the Graphics library).  Exposure events in X11 behave in a
-- similar way except that they do not overtake other events.)

data Events = Events { events :: Chan Event
                     , tick   :: Flag ()
		     }

newEvents :: IO Events
getEvent  :: Events -> IO Event
isNoEvent :: Events -> IO Bool
sendEvent :: Events -> Event -> IO ()
sendTick  :: Events -> IO ()
getTick   :: Events -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

newEvents = do
  events <- newChan 
  tick   <- newFlag
  return (Events { events=events, tick=tick })

getEvent  evs = readChan    (events evs)
isNoEvent evs = isEmptyChan (events evs)
sendEvent evs = writeChan   (events evs)
sendTick  evs = setFlag     (tick evs) ()
getTick   evs = resetFlag   (tick evs)

----------------------------------------------------------------
-- End
----------------------------------------------------------------