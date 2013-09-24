module StdReceiver ( Receivers(..)
                   , closeReceiver
                   , getReceivers
                   , enableReceivers, disableReceivers, getReceiverSelectState
                   , asyncSend, syncSend, syncSend2
                   , module StdReceiverDef
                   ) where


--	********************************************************************************
--	Clean Standard Object I/O library, version 1.2
--	
--	StdReceiver specifies all receiver operations.
--	********************************************************************************


import CommonDef
import DeviceEvents
import Id
import IOState
import OSEvent
import ReceiverAccess
import ReceiverDefAccess
import ReceiverDevice
import ReceiverHandle
import ReceiverId
import ReceiverTable
import Scheduler
import StdReceiverAttribute
import StdReceiverDef
import System.IO(fixIO)
import Data.IORef


stdReceiverFatalError :: String -> String -> x
stdReceiverFatalError rule error
	= dumpFatalError rule "StdReceiver" error


--	Open one-way receiver:

receiverStateIdentified :: Id -> ReceiverStateHandle ps -> Bool
receiverStateIdentified id (ReceiverStateHandle _ rH) = receiverIdentified id rH

class Receivers rdef where
	openReceiver :: ls -> rdef ls ps -> ps -> GUI ps ps


instance Receivers (Receiver m) where
	openReceiver ls rDef ps
		= do {
			ps1 <- dOpen receiverFunctions ps;
			idtable      <-  ioStGetIdTable;
			if   memberIdTable id idtable
			then throwGUI ErrorIdsInUse
			else 
			do {
				rt   <- ioStGetReceiverTable;
				let maybe_parent = getReceiverTableEntry id rt
				in
				if   isJust maybe_parent	-- This condition should not occur: IdTable didn't contain Id while ReceiverTable does.
				then stdReceiverFatalError "openReceiver (Receiver)" "inconsistency detected between IdTable and ReceiverTable"
				else
				do {
					(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
					if   not found		-- This condition should not occur: ReceiverDevice has just been 'installed'
					then stdReceiverFatalError "openReceiver (Receiver)" "could not retrieve ReceiverSystemState from IOSt"
					else
					let  rsHs  = rReceivers (receiverSystemStateGetReceiverHandles rDevice)
					in
					do {
						bindRId id select id ReceiverDevice;
						ioId <- accIOEnv ioStGetIOId;
						ioStSetIdTable (snd (addIdToIdTable id (IdParent {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id}) idtable));
						ps2  <- toGUI (doInitIO (receiverInit (ls,ps1))
						                        (\ls ioState -> ioStSetDevice
						                                            (ReceiverSystemState
						                                                 (ReceiverHandles
						                                                     {rReceivers = (ReceiverStateHandle ls (newReceiverHandle rid select f)) : rsHs}))
						                                            ioState));
						return ps2
					}
				}
			}
		  }
		where
			rid            = receiverDefRId        rDef
			select         = receiverDefSelectState rDef
			f              = receiverDefFunction    rDef
			rDefAttributes = receiverDefAttributes  rDef
			receiverInit   = getReceiverInitFun (snd (cselect isReceiverInit (ReceiverInit return) rDefAttributes))
			id             = rIdtoId rid
			
			
instance Receivers (Receiver2 m r) where
	openReceiver ls rDef ps
		= do {
			ps1 <- dOpen receiverFunctions ps;
			idtable      <-  ioStGetIdTable;
			if   memberIdTable id idtable
			then throwGUI ErrorIdsInUse
			else 
			do {
				rt   <- ioStGetReceiverTable;
				let maybe_parent = getReceiverTableEntry id rt
				in
				if   isJust maybe_parent	-- This condition should not occur: IdTable didn't contain Id while ReceiverTable does.
				then stdReceiverFatalError "openReceiver (Receiver)" "inconsistency detected between IdTable and ReceiverTable"
				else
				do {
					(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
					if   not found		-- This condition should not occur: ReceiverDevice has just been 'installed'
					then stdReceiverFatalError "openReceiver (Receiver)" "could not retrieve ReceiverSystemState from IOSt"
					else
					let  rsHs  = rReceivers (receiverSystemStateGetReceiverHandles rDevice)
					in
					do {
						bindRId id select id ReceiverDevice;
						ioId <- accIOEnv ioStGetIOId;
						ioStSetIdTable (snd (addIdToIdTable id (IdParent {idpIOId=ioId,idpDevice=ReceiverDevice,idpId=id}) idtable));
						ps2  <- toGUI (doInitIO (receiverInit (ls,ps1))
						                        (\ls ioState -> ioStSetDevice
						                                            (ReceiverSystemState
						                                                 (ReceiverHandles
						                                                     {rReceivers = (ReceiverStateHandle ls (newReceiver2Handle rid select f)) : rsHs}))
						                                            ioState));
						return ps2
					}
				}
			}
		  }
		where
			rid            = receiver2DefR2Id        rDef
			select         = receiver2DefSelectState rDef
			f              = receiver2DefFunction    rDef
			rDefAttributes = receiver2DefAttributes  rDef
			receiverInit   = getReceiverInitFun (snd (cselect isReceiverInit (ReceiverInit return) rDefAttributes))
			id             = r2IdtoId rid


doInitIO :: GUI ps (ls,ps) -> (ls -> IOSt ps -> IOSt ps) -> IOSt ps -> IO (ps,IOSt ps)
doInitIO initGUI setReceiverLS ioState
	= do {
		r <- fixIO (\st -> fromGUI initGUI (setReceiverLS (fst (fst st)) ioState));
		let ((_,ps1),ioState1) = r
		in  return (ps1,ioState1)
	  }


--	Closing receivers.

closeReceiver :: Id -> GUI ps ()
closeReceiver id
	= do {
		closed <- accIOEnv ioStClosed;
		if   closed
		then return ()
		else 
		if   not (isCustomRId id || isCustomId id)
		then return ()
		else
		do {
			(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
			if   not found
			then return ()
			else 
			let  rsHs              = rReceivers (receiverSystemStateGetReceiverHandles rDevice)
			     (found,rsH,rsHs1) = remove (receiverStateIdentified id) undefined rsHs
			in
			do {
				appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=rsHs1})));
				if   not found
				then return ()
				else 
				do {
					idtable <- ioStGetIdTable;
					ioStSetIdTable (snd (removeIdFromIdTable id idtable));
					unbindRId id;
				{-	TCP not supported yet
					appIOEnv (ioStSetRcvDisabled True);
					sequence_ [closeReceiver id | id <- rConnected $ rHandle $ rsH];
					case rInetInfo $ rHandle $ rsH of
					     Nothing               -> return ()
					     Just (_,_,_,closefun) -> closefun
				-}
				}
			}
		}
	  }


--	Get the Ids and ReceiverTypes of all current receivers:

getReceivers :: GUI ps [Id]
getReceivers
	= do {
		(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
		if   not found
		then return []
		else return (map  getreceiver (rReceivers (receiverSystemStateGetReceiverHandles rDevice)))
	  }
	where		
		getreceiver :: ReceiverStateHandle ps -> Id
		getreceiver rsH@(ReceiverStateHandle _ (ReceiverHandle {rId=rId})) = rId
		

--	Changing attributes:

enableReceivers :: [Id] -> GUI ps ()
enableReceivers ids
	= changeReceivers (receiverSetSelectState Able) (receiverEntrySetSelectState Able) ids

disableReceivers :: [Id] -> GUI ps ()
disableReceivers ids
	= -- appIOEnv (ioStSetRcvDisabled True) >> (
	     changeReceivers (receiverSetSelectState Unable) (receiverEntrySetSelectState Unable) ids -- )

receiverEntrySetSelectState :: SelectState -> ReceiverTableEntry -> ReceiverTableEntry
receiverEntrySetSelectState select rte
	= rte {rteSelectState=select}

changeReceivers :: IdFun (ReceiverStateHandle ps) -> IdFun ReceiverTableEntry -> [Id] -> GUI ps ()
changeReceivers changeReceiverState changeReceiverEntry ids
	| null okids			-- There aren't any receiver ids in the list
		= return ()
	| otherwise
		= do {
			(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
			if   not found
			then return ()
			else
			let  rsHs          = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
			     allIds        = okids -- getConnectedIds okids [] rsHs rsHs
			     (myids,rsHs1) = changereceiverstates changeReceiverState allIds rsHs
			in
			do {
				appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=rsHs1})));
				if   null myids	-- No receivers were changed
				then return ()
				else ioStGetReceiverTable >>= (\rt -> 
					ioStSetReceiverTable (changereceiverentries changeReceiverEntry myids rt))
			}
		  }
	where
		okids = filter (\id->isCustomRId id || isCustomId id) ids
		
	{-	TCP not supported yet:
		getConnectedIds :: [Id] -> [Id] -> [ReceiverStateHandle ps] -> [ReceiverStateHandle ps] -> [Id]
		getConnectedIds ids _ [] _
			= ids
		getConnectedIds ids alreadyHandled (rsH@(ReceiverStateHandle {rHandle=ReceiverHandle {rId=rId,rConnected=rConnected}}) : rsHs) allStateHandles
			| not (rId `elem` ids) || null rConnected || (rId `elem` alreadyHandled)
				= getConnectedIds ids alreadyHandled rsHs allStateHandles
			| otherwise	-- search again in the whole set of receivers
				= getConnectedIds (nub (rConnected++ids)) (rId:alreadyHandled) allStateHandles allStateHandles
	-}	
		changereceiverstates :: IdFun (ReceiverStateHandle ps) -> [Id] -> [ReceiverStateHandle ps] -> ([Id],[ReceiverStateHandle ps])
		changereceiverstates _ [] _
			= ([],[])
		changereceiverstates _ _ []
			= ([],[])
		changereceiverstates f ids (rsH@(ReceiverStateHandle _ (ReceiverHandle {rId=rId})) : rsHs)
			| hadId       = (rId:rIds, (f rsH): rsHs1)
			| otherwise   = (    rIds,    rsH : rsHs1)
			where
				(hadId,_,ids1) = remove ((==) rId) (dummy "changereceiverstates") ids
				(rIds,rsHs1)   = changereceiverstates f ids1 rsHs
		
		changereceiverentries :: IdFun ReceiverTableEntry -> [Id] -> ReceiverTable -> ReceiverTable
		changereceiverentries f (id:ids) rt
			= case getReceiverTableEntry id rt of
			       Nothing  -> changereceiverentries f ids rt
			       Just rte -> changereceiverentries f ids (setReceiverTableEntry (f rte) rt)
		changereceiverentries _ _ rt
			= rt


--	Get the SelectState of a receiver:

getReceiverSelectState :: Id -> GUI ps (Maybe SelectState)
getReceiverSelectState id
	| not (isCustomRId id || isCustomId id)
		= return Nothing
	| otherwise
		= do {
			(found,rDevice) <- accIOEnv (ioStGetDevice ReceiverDevice);
			if   not found
			then return Nothing
			else 
			let  rsHs           = rReceivers $ receiverSystemStateGetReceiverHandles rDevice
			     (select,rsHs1) = getselectstate id rsHs
			in   appIOEnv (ioStSetDevice (ReceiverSystemState (ReceiverHandles {rReceivers=rsHs1}))) >> return select
		  }
	where
		getselectstate :: Id -> [ReceiverStateHandle ps] -> (Maybe SelectState,[ReceiverStateHandle ps])
		getselectstate id (rsH@(ReceiverStateHandle _ rH@(ReceiverHandle {rSelect=rSelect})) : rsHs)
			| receiverIdentified id rH = (Just rSelect, rsH:rsHs )
			| otherwise                = (select,       rsH:rsHs1)
			where
				(select,rsHs1)     = getselectstate id rsHs
		getselectstate _ _ = (Nothing,[])


--	Message passing:


{-	Asynchronous, uni-directional, message passing.
	If the receiver could not be found in the global ReceiverTable, 
		then return SendUnknownReceiver and do nothing.
	If the receiver could be found, then increase the length of the 
		asynchronous message queue of the receiver in the global 
		ReceiverTable.
		Add the message to the asynchronous message queue of the 
		receiver using the scheduler.
-}

asyncSend :: RId msg -> msg -> GUI ps ()
asyncSend rid msg = do
	rt <- ioStGetReceiverTable
	(case getReceiverTableEntry (rIdtoId rid) rt of
		Nothing  -> throwGUI ErrorUnknownObject
		Just rte -> do
			liftIO (modifyIORef (getRIdIn rid) ((:) msg))	-- Put the msg in the async queue of the receiver
			ioStAppendEvents [ScheduleMsgEvent (rteLoc rte)]
			return ())

{-	Synchronous message passing.
	If the receiver could not be found in the global ReceiverTable,
		then raise ErrorUnknownObject exception.
	If the receiver could be found, then let the receiver handle the
		synchronous message using the scheduler.
-}
syncSend :: RId msg -> msg -> ps -> GUI ps ps
syncSend rid msg ps = do
	rt <- ioStGetReceiverTable
	(case getReceiverTableEntry (rIdtoId rid) rt of
	     Nothing  -> throwGUI ErrorUnknownObject
	     Just rte -> do
		liftIO (modifyIORef (getRIdIn rid) ((:) msg))	-- Put the msg in the async queue of the receiver
		(_,ps) <- liftContextIO (handleMsgEvent rte) ps
		return ps)

{-	Synchronous bi-directional message passing.
	If the receiver could not be found in the global ReceiverTable,
		then raise ErrorUnknownObject exception.
	If the receiver could be found, then let the receiver handle the
		synchronous message using the scheduler.
-}
syncSend2 :: R2Id msg resp -> msg -> ps -> GUI ps (resp,ps)
syncSend2 rid msg ps = do
	rt <- ioStGetReceiverTable
	(case getReceiverTableEntry (r2IdtoId rid) rt of
	     Nothing  -> throwGUI ErrorUnknownObject
	     Just rte -> do
		liftIO (writeIORef (getR2IdIn rid) msg)	-- Put the msg in the input of the receiver
		(_,ps) <- liftContextIO (handleMsgEvent rte) ps
		out <- liftIO (readIORef (getR2IdOut rid))
		liftIO (writeIORef (getR2IdOut rid) undefined)  -- clearing receiver output
		return (out,ps))

handleMsgEvent :: ReceiverTableEntry -> Context -> IO [Int]
handleMsgEvent rte context = handleContextOSEvent context (ScheduleMsgEvent (rteLoc rte))
