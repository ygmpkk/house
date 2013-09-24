module ReceiverEvent where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	ReceiverEvent defines the DeviceEventFunction for the receiver device.
--	This function is placed in a separate module because it is platform dependent.
--	********************************************************************************


import DeviceEvents
import IOState
import OSEvent



{-	receiverEvent filters the appropriate events for the receiver device.
	These are only the message events (as long as receivers do not contain timers).
	receiverEvent assumes that it is not applied to an empty IOSt.
	
	Currently, in this implementation only asynchronous message events are supported.
-}

receiverEvent :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)

receiverEvent ioState schedulerEvent@(ScheduleMsgEvent recLoc)
    | ioStGetIOId ioState == rlIOId recLoc && ReceiverDevice==rlDevice recLoc
        = return (True,Just (ReceiverEvent recLoc),schedulerEvent)
    | otherwise
        = return (False,Nothing,schedulerEvent)

receiverEvent ioState schedulerEvent = return (False,Nothing,schedulerEvent)
