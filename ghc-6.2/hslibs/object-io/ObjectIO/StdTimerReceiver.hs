module StdTimerReceiver where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	********************************************************************************

import	CommonDef
import	Id
import	ReceiverAccess
import	ReceiverDefAccess
import	StdReceiverAttribute
import	StdTimerDef
import	StdTimerElementClass
import	TimerHandle


instance TimerElements (Receiver m) where	
	timerElementToHandles (Receiver rid f atts) =
		return [timerElementHandleToTimerElementState
				(TimerReceiverHandle (newReceiverHandle rid (getSelectState atts) f)
						     (TimerId (rIdtoId rid):map receiverAttToTimerAtt atts))
			]
			
instance TimerElements (Receiver2 m r) where	
	timerElementToHandles (Receiver2 rid f atts) =
		return [timerElementHandleToTimerElementState
				(TimerReceiverHandle (newReceiver2Handle rid (getSelectState atts) f)
						     (TimerId (r2IdtoId rid):map receiverAttToTimerAtt atts))
			]


receiverAttToTimerAtt :: ReceiverAttribute ls ps -> TimerAttribute ls ps
receiverAttToTimerAtt (ReceiverSelectState s) = TimerSelectState s

getSelectState :: [ReceiverAttribute ls ps] -> SelectState
getSelectState rAtts = getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) rAtts))
