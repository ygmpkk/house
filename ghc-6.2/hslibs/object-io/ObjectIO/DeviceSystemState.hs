module DeviceSystemState (module DeviceSystemState, module Device) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	DeviceSystemState defines the device administration types and unwrapper 
--	functions.
--	********************************************************************************


import Device
import ReceiverHandle
import WindowHandle
import TimerHandle
import MenuHandle


data	DeviceSystemState ps
	= MenuSystemState	(MenuHandles		ps)
	| ReceiverSystemState   (ReceiverHandles 	ps)
	| TimerSystemState	(TimerHandles		ps)
	| WindowSystemState     (WindowHandles 		ps)

toDevice :: DeviceSystemState ps -> Device
toDevice (ReceiverSystemState _) = ReceiverDevice
toDevice (WindowSystemState   _) = WindowDevice
toDevice (TimerSystemState   _)  = TimerDevice
toDevice (MenuSystemState     _) = MenuDevice


receiverSystemStateGetReceiverHandles :: DeviceSystemState ps -> ReceiverHandles ps
receiverSystemStateGetReceiverHandles (ReceiverSystemState rsHs) = rsHs 

windowSystemStateGetWindowHandles :: DeviceSystemState ps -> WindowHandles ps
windowSystemStateGetWindowHandles (WindowSystemState wsHs) = wsHs

timerSystemStateGetTimerHandles :: DeviceSystemState ps -> TimerHandles ps
timerSystemStateGetTimerHandles (TimerSystemState tsHs) = tsHs

menuSystemStateGetMenuHandles :: DeviceSystemState ps -> MenuHandles ps
menuSystemStateGetMenuHandles (MenuSystemState msHs) = msHs

