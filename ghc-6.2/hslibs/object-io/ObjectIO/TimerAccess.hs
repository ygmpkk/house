module TimerAccess where


--	Clean Object I/O library, version 1.2


import  Id
import	CommonDef
import	DeviceSystemState
import	TimerHandle
import	TimerTable
import  ReceiverTable
import  ReceiverHandle


timerAccessFatalError :: String -> String -> x
timerAccessFatalError function error = dumpFatalError function "ÒimerÀccess" error


{-	bindTimerElementIds binds all unbound R(2)Ids and Ids that can be located in the list of TimerElementStates.
	The Boolean result is True only if no bound identification was found, otherwise it is False.
-}
bindTimerElementIds :: SystemId -> Id -> [TimerElementHandle ls ps] -> ReceiverTable -> IdTable -> (Bool,ReceiverTable,IdTable)
bindTimerElementIds pid timerid (itemH:itemHs) rt it =
	let (ok,rt1,it1) = bindTimerElementIds' pid timerid itemH rt it
	in if not ok then (ok,rt1,it1)
	   else bindTimerElementIds pid timerid itemHs rt1 it1
	where
		bindTimerElementIds' :: SystemId -> Id -> TimerElementHandle ls ps -> ReceiverTable -> IdTable -> (Bool, ReceiverTable,IdTable)
		bindTimerElementIds' pid timerid itemH@(TimerReceiverHandle trH _) rt it
			| memberIdTable rid it = (False,rt,it)
			| isJust (getReceiverTableEntry rid rt) =
				timerAccessFatalError "bindTimerElementIds" "inconsistency detected between IdTable and ReceiverTable"
			| otherwise = (True,rt1,it1)
			where
				rl	= RecLoc {rlIOId=pid,rlDevice=TimerDevice,rlParentId=timerid,rlReceiverId=rid}
				rte	= ReceiverTableEntry{rteLoc=rl,rteSelectState=rSelect trH}
				(_,rt1)	= addReceiverToReceiverTable rte rt
				(_,it1)	= addIdToIdTable rid (IdParent{idpIOId=pid,idpDevice=TimerDevice,idpId=timerid}) it
				rid	= rId trH

		bindTimerElementIds' pid timerid (TimerListLSHandle itemHs) rt it =
			bindTimerElementIds pid timerid itemHs rt it

		bindTimerElementIds' pid timerid (TimerExtendLSHandle exLS itemHs) rt it =
			bindTimerElementIds pid timerid itemHs rt it

		bindTimerElementIds' pid timerid (TimerChangeLSHandle chLS itemHs) rt it =
			bindTimerElementIds pid timerid itemHs rt it
		
bindTimerElementIds _ _ itemHs rt it = (True,rt,it)


{-	unbindTimerElementIds unbinds all bound R(2)Ids and Ids that can be located in the list of TimerElementStates. -}

unbindTimerElementIds :: SystemId -> [TimerElementHandle ls ps] -> (TimerTable,ReceiverTable,IdTable) -> (TimerTable,ReceiverTable,IdTable)
unbindTimerElementIds pid itemHs tables
	= foldr unbindTimerElementIds' tables itemHs
	where
		unbindTimerElementIds' :: TimerElementHandle ls ps -> (TimerTable,ReceiverTable,IdTable) -> (TimerTable,ReceiverTable,IdTable)
		unbindTimerElementIds' (TimerReceiverHandle (ReceiverHandle {rId=rId}) _) (tt,rt,it) =
			(snd (removeTimerFromTimerTable teLoc tt),snd (removeReceiverFromReceiverTable rId rt),snd (removeIdFromIdTable rId it))
			where
				teLoc	= TimerLoc{tlIOId=pid,tlDevice=TimerDevice,tlParentId=rId,tlTimerId=rId}
		unbindTimerElementIds' (TimerListLSHandle itemHs) tables =
			foldr unbindTimerElementIds' tables itemHs
		unbindTimerElementIds' (TimerExtendLSHandle exLS itemHs) tables =
			foldr unbindTimerElementIds' tables itemHs
		unbindTimerElementIds' (TimerChangeLSHandle chLS itemHs) tables =
			foldr unbindTimerElementIds' tables itemHs

identifyTimerStateHandle :: Id -> TimerStateHandle ps -> Bool
identifyTimerStateHandle id (TimerStateHandle (TimerLSHandle {tHandle=tH})) = id == tId tH
