module ReceiverHandle where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	ReceiverHandle contains the internal data structures that represent the 
--	state of receivers. 
--	********************************************************************************


import Id
import StdReceiverDef
--import IORef

         -- Compile with -cpp; set MVAR=1 for MVAR version


data	ReceiverHandles ps
 =	ReceiverHandles
 		{ rReceivers :: [ReceiverStateHandle ps]
		}
data	ReceiverStateHandle ps
 =	forall ls .
	ReceiverStateHandle 
		ls					-- The local state of the receiver
		(ReceiverHandle ls ps)			-- The receiver handle

data	ReceiverHandle ls ps
 =	ReceiverHandle
		{ rId           :: Id			-- The id of the receiver
		, rSelect       :: SelectState		-- The current SelectState of the receiver
		, rFun          :: GUIFun ls ps
		}


receiverIdentified :: Id -> ReceiverHandle ls ps -> Bool
receiverIdentified id rH = id==rId rH

receiverSetSelectState :: SelectState -> ReceiverStateHandle ps -> ReceiverStateHandle ps
receiverSetSelectState select rsH@(ReceiverStateHandle ls rH) = ReceiverStateHandle ls (rH {rSelect=select})
