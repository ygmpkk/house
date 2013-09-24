module ReceiverAccess ( newReceiverHandle, newReceiver2Handle ) where


--	********************************************************************************
--	Clean Standard Object I/O library, version 1.2
--	
--	ReceiverAccess contains ReceiverHandle access.
--	********************************************************************************


import Id
import ReceiverDefAccess
import ReceiverHandle
import IOState(liftIO)
import Data.IORef


newReceiver2Handle :: R2Id m r -> SelectState -> Receiver2Function m r ls ps -> ReceiverHandle ls ps
newReceiver2Handle rid select f
	= ReceiverHandle
		{ rId        = r2IdtoId   rid
		, rSelect    = select
		, rFun       = receiverFunction
		}
	where
		input  = getR2IdIn  rid
		output = getR2IdOut rid

		receiverFunction ls_ps = do
			msg <- liftIO (readIORef input)
			liftIO (writeIORef input undefined)
			(resp, ls_ps) <- f msg ls_ps
			liftIO (writeIORef output resp)
			return ls_ps

newReceiverHandle :: RId m -> SelectState -> ReceiverFunction m ls ps -> ReceiverHandle ls ps
newReceiverHandle rid select f
	= ReceiverHandle
		{ rId        = rIdtoId   rid
		, rSelect    = select
		, rFun       = receiverFunction
		}
	where
		inChan  = getRIdIn  rid

		receiverFunction ls_ps = do
			(msg:msgs) <- liftIO (readIORef inChan)
			liftIO (writeIORef inChan msgs)
			ls_ps <- f msg ls_ps
			return ls_ps