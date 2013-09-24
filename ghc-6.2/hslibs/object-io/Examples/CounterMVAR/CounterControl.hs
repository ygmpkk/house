module CounterControl where


--	********************************************************************************
--	CounterControl defines a new instance of the Controls class.
--	A CounterControl is a control that displays an integer value that can be
--	manually incremented or decremented using two buttons.
--	********************************************************************************


import StdIO
import Concurrent


data	CounterControl
 =	CounterControl Int

instance Controls CounterControl where
	controlToHandles (CounterControl value)
		= do {
			displayId <- openId;
			ls        <- liftIO (newMVar value);
			controlToHandles 
				(	TextControl (show value) [ControlId displayId]
				:+:	ButtonControl "-" [ControlFunction (f (-1) displayId ls),ControlPos (Below displayId,zero)]
				:+:	ButtonControl "+" [ControlFunction (f   1  displayId ls)]
				)
		  }
		where
			f dx displayId ls
				= do {
					value <- liftIO (takeMVar ls);
					liftIO (putMVar ls (value+dx));
					setControlText displayId (show (value+dx))
				  }
