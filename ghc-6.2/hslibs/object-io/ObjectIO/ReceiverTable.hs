module ReceiverTable ( ReceiverTable
                     , ReceiverTableEntry(..), RecLoc(..)
                     , initialReceiverTable
                     , addReceiverToReceiverTable
                     , removeReceiverFromReceiverTable
                     , getReceiverTableEntry
                     , setReceiverTableEntry
--                   , getActiveReceiverTableEntry
                     ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	ReceiverTable registers all open receivers.
--	********************************************************************************


import CommonDef
import Device
import Id
import Maybe
import SystemId


type	ReceiverTable					-- The table of all receivers
 =	[ReceiverTableEntry]
data	ReceiverTableEntry
 =	ReceiverTableEntry
 		{ rteLoc         :: !RecLoc		-- The location of the receiver
		, rteSelectState :: !SelectState	-- The current SelectState of the receiver
		}
data	RecLoc
 =	RecLoc
 		{ rlIOId         :: !SystemId		-- Id of parent process
		, rlDevice       :: !Device		-- Device kind of parent
		, rlParentId     :: !Id			-- Id of parent device instance
		, rlReceiverId   :: !Id			-- R(2)Id of the receiver itself
		}


initialReceiverTable :: ReceiverTable			-- initialReceiverTable yields an empty ReceiverTable
initialReceiverTable = []

eqReceiverId :: Id -> RecLoc -> Bool			-- receivers are added uniquely by their rlReceiverId
eqReceiverId id recLoc
	= id==rlReceiverId recLoc


{-	addReceiverToReceiverTable adds a new receiver entry to the ReceiverTable.
	The Boolean result is True iff no duplicate receiver entry was found, otherwise it is False.
-}
addReceiverToReceiverTable :: ReceiverTableEntry -> ReceiverTable -> (Bool,ReceiverTable)
addReceiverToReceiverTable rte@(ReceiverTableEntry {rteLoc=RecLoc {rlReceiverId=rlReceiverId}}) receivers
	= add rlReceiverId rte receivers
	where
		add :: Id -> ReceiverTableEntry -> [ReceiverTableEntry] -> (Bool,[ReceiverTableEntry])
		add rid entry (rte@(ReceiverTableEntry {rteLoc=rteLoc}) : rtes)
			| eqReceiverId rid rteLoc
				= (False,rte:rtes)
			| otherwise
				= let (isnew,rtes1) = add rid entry rtes
				  in  (isnew,rte:rtes1)
		add _ entry _
			= (True,[entry])


{-	removeReceiverFromReceiverTable removes a receiver identified by Id from the ReceiverTable.
	The Boolean result is True iff an entry was actually removed, otherwise it is False.
-}
removeReceiverFromReceiverTable :: Id -> ReceiverTable -> (Bool,ReceiverTable)
removeReceiverFromReceiverTable rid receivers
	= (found,receivers')
	where
		(found,_,receivers') = remove (\rte->eqReceiverId rid (rteLoc rte)) undefined receivers


{-	getReceiverTableEntry returns the receiver identified by Id from the ReceiverTable.
	If such a receiver could be found, then (Just ReceiverTableEntry) is returned, otherwise Nothing.
-}
getReceiverTableEntry :: Id -> ReceiverTable -> Maybe ReceiverTableEntry
getReceiverTableEntry rid (rte@(ReceiverTableEntry {rteLoc=rteLoc}):rtes)
	| eqReceiverId rid rteLoc = Just rte
	| otherwise               = getReceiverTableEntry rid rtes
getReceiverTableEntry _ _
	= Nothing


{-	setReceiverTableEntry replaces the current ReceiverTableEntry that has an identical Id. 
	If such an entry could not be found, then the new entry is added behind all other entries.
-}
setReceiverTableEntry :: ReceiverTableEntry -> ReceiverTable -> ReceiverTable
setReceiverTableEntry rte@(ReceiverTableEntry {rteLoc=RecLoc {rlReceiverId=rlReceiverId}}) receivers
	= set rlReceiverId rte receivers
	where
		set :: Id -> ReceiverTableEntry -> [ReceiverTableEntry] -> [ReceiverTableEntry]
		set rid new (rte@(ReceiverTableEntry {rteLoc=rteLoc}) : rtes)
			| eqReceiverId rid rteLoc = new:rtes
			| otherwise               = rte : set rid new rtes
 		set _ new _ = [new]