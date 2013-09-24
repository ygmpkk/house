module TimerEvent(timerEvent) where


--	Clean Object I/O library, version 1.2

import  OSEvent
import  OSTime
import	DeviceEvents
import  DeviceSystemState
import	TimerAccess
import	TimerTable
import	TimerHandle(TimerHandles(..))
import  ReceiverId
import	CommonDef
import	IOState
import	ClCrossCall_12
import  Data.IORef

timerEventFatalError :: String -> String -> x
timerEventFatalError function error = dumpFatalError function "TimerEvent" error


{-	The timerEvent function determines whether the given SchedulerEvent can be applied
	to a timer of this process. These are the following cases:
	*	ScheduleTimerEvent: the timer event belongs to this process and device
	*	ScheduleMsgEvent:   the message event belongs to this process and device
	timerEvent assumes that it is not applied to an empty IOSt.
-}
timerEvent :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)
timerEvent ioState schedulerEvent = do
	(if not (ioStHasDevice TimerDevice ioState)
	 then timerEventFatalError "timerFunctions.dEvent" "could not retrieve TimerSystemState from IOSt"
	 else timerEvent schedulerEvent)
	where
		timerEvent :: SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)
		timerEvent schedulerEvent@(ScheduleOSEvent osEvent@(CrossCallInfo {ccMsg=ccMsg}) _)
			| ccMsg == ccWmIDLETIMER || ccMsg == ccWmTIMER = do
				iocontext <- readIORef (ioStGetContext ioState)
				let tt = ioContextGetTimerTable iocontext
				let t1 = ioContextGetTime iocontext
				t2 <- osGetTime
				let tt1 = shiftTimeInTimerTable (t2-t1) tt
				let (mb_te, tt2) = getActiveTimerInTimerTable tt1
				(case mb_te of
					Just te | tlIOId (teLoc te) == ioStGetIOId ioState -> do
						let iocontext1 = ioContextSetTimerTable tt2 iocontext
						let iocontext2 = ioContextSetTime t2 iocontext1
						writeIORef (ioStGetContext ioState) iocontext2
						return (True, Just (TimerDeviceEvent te),schedulerEvent)
					_ -> return (False,Nothing,schedulerEvent))
			| otherwise = return (False,Nothing,schedulerEvent)

		timerEvent schedulerEvent@(ScheduleMsgEvent recLoc)
			| rlIOId recLoc == ioStGetIOId ioState && rlDevice recLoc == TimerDevice =
				return (True,Just (ReceiverEvent recLoc),schedulerEvent)
			| otherwise =
				return (False,Nothing,schedulerEvent)
