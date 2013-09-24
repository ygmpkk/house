module ReceiverDevice (receiverFunctions) where


--	********************************************************************************
--	Clean Standard Object I/O library, version 1.2
--	
--	ReceiverDevice defines the receiver device event handlers.
--	********************************************************************************


import CommonDef
import Id
import IOState
import ReceiverEvent
import ReceiverHandle
import ReceiverId
import ReceiverTable
import System.IO(fixIO)


receiverdeviceFatalError :: String -> String -> x
receiverdeviceFatalError rule error
	= dumpFatalError rule "ReceiverDevice" error

receiverFunctions :: DeviceFunctions ps
receiverFunctions
	= DeviceFunctions
		{ dDevice = ReceiverDevice
		, dShow	  = return
		, dHide	  = return
		, dEvent  = receiverEvent
		, dDoIO   = receiverIO
		, dOpen   = receiverOpen
		, dClose  = receiverClose
		}

receiverOpen :: ps -> GUI ps ps
receiverOpen ps
	= do {
		hasReceiver <- accIOEnv (ioStHasDevice ReceiverDevice);
		if   hasReceiver
		then return ps
		else do {
			appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=[]})));
			appIOEnv (ioStSetDeviceFunctions receiverFunctions);
			return ps
		     }
	  }

receiverClose :: ps -> GUI ps ps
receiverClose ps
	= do {
{-	TCP not handled yet
		callReceiverCloseFunctions;
		appIOEnv (ioStSetRcvDisabled True);
-}
		(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
		if   not found
		then return ps
		else 
		let  rHs  = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
		     rIds = map (\(ReceiverStateHandle _ rH) -> rId rH) rHs
		in
		do {
			idtable <- ioStGetIdTable;
			ioStSetIdTable (snd (removeIdsFromIdTable rIds idtable));
			unbindRIds rIds;
			appIOEnv (ioStRemoveDevice ReceiverDevice);
			appIOEnv (ioStRemoveDeviceFunctions ReceiverDevice);
			return ps
		}
	  }
{-	where
		callReceiverCloseFunctions :: GUI ps ()
		callReceiverCloseFunctions
			= do {
				(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
				if   not found
				then return ()
				else
				let  rHs = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
				in   sequence_ (map callCloseFunc rHs)
			  }
			where
				callCloseFunc {rHandle={rInetInfo=Nothing, rConnected}}
					= sequence_ (map closeReceiver rConnected)
				callCloseFunc {rHandle={rInetInfo=Just (_,_,_,closeFun), rConnected}}
					= sequence_ (map closeReceiver rConnected) >> closeFun
-}

{-	The receiver handles one message event:
	- ASyncMessage:
		this is a request to handle the first asynchronous message available in the
		asynchronous message queue. Globally the size of the asynchronous message queue
		has already been decreased.
	The QASyncMessage has become superfluous as it is replaced by Channel operations.
	The SyncMessage is currently ignored as synchronous message passing is not yet implemented.
-}
receiverIO :: DeviceEvent -> ps -> GUI ps ps
receiverIO deviceEvent ps
	= do {
		(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
		if   not found		-- This condition should not occur: dDoIO function should be applied only iff dEvent filters message
		then receiverdeviceFatalError "receiverIO" "could not retrieve ReceiverSystemState from IOSt"
		else 
		let  rsHs = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
		in   receiverIO deviceEvent rsHs ps
	  }
	where
		receiverIO :: DeviceEvent -> [ReceiverStateHandle ps] -> ps -> GUI ps ps
		
		receiverIO (ReceiverEvent recLoc) rsHs ps =
			letOneReceiverDoIO recLoc rsHs ps
			where
				letOneReceiverDoIO :: RecLoc -> [ReceiverStateHandle ps] -> ps -> GUI ps ps
				letOneReceiverDoIO recLoc rsHs ps
					= toGUI (letReceiverDoIO rsH rsHs1 ps)
					where
						dummy           = receiverdeviceFatalError "receiverIO (ReceiverEvent (ASyncMessage _))" "receiver could not be found"
						(_,rsH,rsHs1)   = remove (identifyReceiverStateHandle (rlParentId recLoc)) dummy rsHs
						
						letReceiverDoIO :: ReceiverStateHandle ps -> [ReceiverStateHandle ps] -> ps -> IOSt ps -> IO (ps,IOSt ps)
						letReceiverDoIO (ReceiverStateHandle ls rH@(ReceiverHandle {rFun=rFun})) rsHs ps ioState
							= do {
								r   <- fixIO (\st -> fromGUI (rFun (ls,ps))
								                             (ioStSetDevice 
								                                 (ReceiverSystemState
								                                     (ReceiverHandles 
								                                         {rReceivers=rsHs ++ [ReceiverStateHandle (fst (fst st)) rH]}))
								                                 ioState));
								let ((_,ps1),ioState1) = r
								in  return (ps1,ioState1)
							  }

		receiverIO _ _ _ = receiverdeviceFatalError "receiverIO" "device event passed receiver event filter without handling"


identifyReceiverStateHandle :: Id -> ReceiverStateHandle ps -> Bool
identifyReceiverStateHandle id (ReceiverStateHandle _ (ReceiverHandle {rId=rId})) = id==rId
