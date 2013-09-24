module ReceiverId where


--	********************************************************************************
--	Clean Standard Object I/O library, version 1.2
--	
--	ReceiverId contains the operations to bind and unbind receivers in the 
--	ReceiverTable.
--	********************************************************************************

import Id
import StdIOCommon(SelectState(..))
import IOState
import ReceiverTable


bindRId :: Id -> SelectState -> Id -> Device -> GUI ps ()
bindRId rid select deviceid device
	= do {
		pid <- accIOEnv ioStGetIOId;
		rt  <- ioStGetReceiverTable;
		let rl = RecLoc
				{ rlIOId       = pid
				, rlDevice     = device
				, rlParentId   = deviceid
				, rlReceiverId = rid
				}
		in  ioStSetReceiverTable (snd (addReceiverToReceiverTable (ReceiverTableEntry {rteLoc=rl,rteSelectState=select}) rt))
	  }

unbindRId :: Id -> GUI ps ()
unbindRId rid
	= do {
		rt <- ioStGetReceiverTable;
		ioStSetReceiverTable (snd (removeReceiverFromReceiverTable rid rt))
	  }

unbindRIds :: [Id] -> GUI ps ()
unbindRIds ids
	= do {
		rt <- ioStGetReceiverTable;
		ioStSetReceiverTable (unbindRIds' ids rt)
	  }
	where
		unbindRIds' :: [Id] -> ReceiverTable -> ReceiverTable
		unbindRIds' (rid:rids) rt = unbindRIds' rids (snd (removeReceiverFromReceiverTable rid rt))
		unbindRIds' _          rt = rt
