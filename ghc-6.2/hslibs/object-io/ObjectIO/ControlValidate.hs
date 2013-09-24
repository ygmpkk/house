module ControlValidate ( validateControlTitle, validateSliderState
		       , getWElementControlIds
                       , noDuplicateControlIds, disjointControlIds
                       , controlIdsAreConsistent
                       ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	ControlValidate contains control validation functions.
--	********************************************************************************


import CommonDef
import Id
import OSWindow
import WindowHandle
import ReceiverTable
import ReceiverHandle


controlvalidateFatalError :: String -> String -> x
controlvalidateFatalError function error
	= dumpFatalError function "ControlValidate" error


--	Validate the title of a control.

validateControlTitle :: String -> String
validateControlTitle string
	= removeSpecialChars osControlTitleSpecialChars string


--	Validate the settings of a slider.

validateSliderState :: SliderState -> SliderState
validateSliderState (SliderState{sliderMin=sMin, sliderMax=sMax, sliderThumb=thumb})
	= SliderState
	  { sliderMin	= min'
	  , sliderMax	= max'
	  , sliderThumb	= setBetween thumb min' max'
	  }
	where
		min' = min sMin sMax
		max' = max sMin sMax
	
--	Collect all Ids of the given [WElementHandle].

getWElementControlIds :: [WElementHandle ls ps] -> [Id]
getWElementControlIds [] = []
getWElementControlIds (itemH:itemHs) = ids1++ids2
	where
		ids1 = getWElementIds itemH
		ids2 = getWElementControlIds itemHs
		

		getWElementIds :: (WElementHandle ls ps) -> [Id]
		getWElementIds (WItemHandle {wItemId=id,wItems=itemHs}) = maybeToList id ++ getWElementControlIds itemHs
		getWElementIds (WListLSHandle itemHs) = getWElementControlIds itemHs
		getWElementIds (WExtendLSHandle addLS itemHs) = getWElementControlIds itemHs
		getWElementIds (WChangeLSHandle newLS itemHs) = getWElementControlIds itemHs			  


--	Id occurrence checks on [WElementHandle ls ps] and [WElementHandle`].

--	There are no duplicate (ControlId id) attributes:


noDuplicateControlIds :: [WElementHandle ls ps] -> Bool
noDuplicateControlIds itemHs = noDuplicates (getWElementControlIds itemHs)
	  

--	The list of Ids does not occur in any (ControlId id) attribute:

disjointControlIds :: [Id] -> [WElementHandle ls ps] -> Bool
disjointControlIds ids itemHs = disjointLists ids (getWElementControlIds itemHs)


{-	Bind all free RIds that are contained in the WElementHandles.
	It assumes that it has already been checked that no RId is already bound in the ReceiverTable.
-}
bindReceiverControlIds :: SystemId -> Id -> [WElementHandle ls ps] -> ReceiverTable -> ReceiverTable
bindReceiverControlIds _      _ []             rt = rt
bindReceiverControlIds ioId wId (itemH:itemHs) rt =
	let 
		rt1 = bindReceiverControlIds' ioId wId itemH  rt
	    	rt2 = bindReceiverControlIds  ioId wId itemHs rt1
	in 
		rt2
	where
		bindReceiverControlIds' :: SystemId -> Id -> WElementHandle ls ps -> ReceiverTable -> ReceiverTable
		bindReceiverControlIds' ioId wId itemH@(WItemHandle {wItemKind=wItemKind,wItemInfo=wItemInfo}) rt
			| wItemKind /= IsReceiverControl =
				bindReceiverControlIds ioId wId (wItems itemH) rt
			| otherwise =
				let
					recLoc	= RecLoc{rlIOId=ioId,rlDevice=WindowDevice,rlParentId=wId,rlReceiverId=id}
					rte	= ReceiverTableEntry{rteLoc=recLoc,rteSelectState=if wItemSelect itemH then Able else Unable}
				in 
					snd (addReceiverToReceiverTable rte rt)
			where				
				id = case wItemInfo of 
				   	WReceiverInfo rH -> rId rH

		bindReceiverControlIds' ioId wId (WListLSHandle itemHs) rt =
			bindReceiverControlIds ioId wId itemHs rt			

		bindReceiverControlIds' ioId wId (WExtendLSHandle exLS itemHs) rt =
			bindReceiverControlIds ioId wId itemHs rt			

		bindReceiverControlIds' ioId wId (WChangeLSHandle chLS itemHs) rt =
			bindReceiverControlIds ioId wId itemHs rt
			
	
{-	controlIdsAreConsistent checks whether the WElementHandles contain (R(2))Ids that have already been
	associated with open receivers or other I/O objects and if there are no duplicate Ids. 
	The ReceiverTable is not changed if there are duplicate (R(2))Ids; otherwise all (R(2))Ids have been bound.
-}

controlIdsAreConsistent :: SystemId -> Id -> [WElementHandle ls ps] -> ReceiverTable -> IdTable -> Maybe (IdTable, ReceiverTable)
controlIdsAreConsistent ioId wId itemHs rt it
	| not (okMembersIdTable ids it) = Nothing
	| not ok = controlvalidateFatalError "controlIdsAreConsistent" "could not add all Ids to IdTable"
	| otherwise = Just (it1, rt1)
	where
		ids 	      = getWElementControlIds itemHs
		idParent      = IdParent {idpIOId=ioId,idpDevice=WindowDevice,idpId=wId}
		(ok,it1)      = addIdsToIdTable [(id,idParent) | id<-ids] it
		rt1   	      = bindReceiverControlIds ioId wId itemHs rt