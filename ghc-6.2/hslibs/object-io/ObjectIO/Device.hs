module Device where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	Device defines the set of devices and their priority.
--	********************************************************************************

data	Device				-- The set of devices
	= TimerDevice
	| MenuDevice
	| WindowDevice
	| ReceiverDevice
	| ProcessDevice
	deriving (Eq,Show)

priorityDevice :: Device -> Int
priorityDevice ReceiverDevice = 6
priorityDevice TimerDevice    = 4
priorityDevice MenuDevice     = 3
priorityDevice WindowDevice   = 2
priorityDevice ProcessDevice  = 0

devices = [ ReceiverDevice
	  , TimerDevice
	  , MenuDevice
	  , WindowDevice
	  , ProcessDevice
	  ]
