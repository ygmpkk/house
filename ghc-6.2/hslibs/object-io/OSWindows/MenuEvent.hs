module MenuEvent(menuEvent, menuHandlesGetMenuStateHandles) where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--
--	MenuEvent defines the DeviceEventFunction for the menu device.
--	This function is placed in a separate module because it is platform dependent.
--	********************************************************************************

import  Data.Bits		-- Defines .&. for bitand;
import  Data.Int		-- Defines Int32 instance for Bits;
import  Foreign.C.String
import	ClCrossCall_12
import	ClCCall_12
import  Cutil_12(addr2int)
import	OSMenu(osMenuItemCheck)
import	CommonDef
import	DeviceEvents
import  DeviceSystemState
import	IOState
import  StdGUI
import  OSEvent(SchedulerEvent(..), OSEvent)
import  OSToolBar(OSToolbar(..))
import  OSMenu
import  OSTypes
import  MenuHandle
import	MenuAccess(menuStateHandleGetHandle, menuStateHandleGetMenuId)
import	StdProcessAttribute(getProcessToolbarAtt, isProcessToolbar)
import	ReceiverTable(RecLoc(..))


menuEventFatalError :: String -> String -> x
menuEventFatalError function error
	= dumpFatalError function "MenuEvent" error


{-	menuEvent filters the scheduler events that can be handled by this menu device.
	For the time being no timer menu elements are added, so these events are ignored.
	menuEvent assumes that it is not applied to an empty IOSt and that its device is
	present.
-}
menuEvent :: IOSt ps -> SchedulerEvent -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)
menuEvent ioState schedulerEvent
    | ioStHasDevice MenuDevice ioState	= menuEvent schedulerEvent ioState 
    | otherwise 			= menuEventFatalError "menuEvent" "MenuDevice.dEvent applied while MenuDevice not present in IOSt"       
    where
	menuEvent :: SchedulerEvent -> IOSt ps -> IO (Bool,Maybe DeviceEvent,SchedulerEvent)
	menuEvent schedulerEvent@(ScheduleOSEvent osEvent@(CrossCallInfo {ccMsg=ccMsg}) _) ioState
		| isToolbarOSEvent ccMsg = do			
			(myEvent,replyToOS,deviceEvent) <- filterToolbarEvent (ioStGetOSDInfo ioState) osEvent ioState
			let schedulerEvent1 =
				if isJust replyToOS then
				 	(ScheduleOSEvent osEvent (fromJust replyToOS))
				else schedulerEvent
			return (myEvent,deviceEvent,schedulerEvent1)
		| isMenuOSEvent ccMsg = do
			let (found,mDevice) = ioStGetDevice MenuDevice ioState
			let menus = menuSystemStateGetMenuHandles mDevice
			(myEvent,replyToOS,deviceEvent) <- filterOSEvent osEvent (found {-&& systemId==ioId-}) menus
			let schedulerEvent1 = if isJust replyToOS then (ScheduleOSEvent osEvent (fromJust replyToOS)) else schedulerEvent
			return (myEvent,deviceEvent,schedulerEvent1)
		| otherwise =
			return (False,Nothing,schedulerEvent)
		where
			isMenuOSEvent :: Int -> Bool
			isMenuOSEvent msg = msg == ccWmCOMMAND

			isToolbarOSEvent :: Int -> Bool
			isToolbarOSEvent msg = msg == ccWmBUTTONCLICKED || msg == ccWmGETTOOLBARTIPTEXT
	
	menuEvent schedulerEvent@(ScheduleMsgEvent recLoc) ioState
	       | ioStGetIOId ioState /= (rlIOId recLoc) || (rlDevice recLoc) /= MenuDevice = 
	       		return (False,Nothing,schedulerEvent)
	       | otherwise =
	    	    let (_,mDevice) = ioStGetDevice MenuDevice ioState
		        menus = menuSystemStateGetMenuHandles mDevice
		        found = hasMenuHandlesMenu (rlParentId recLoc) menus
		        deviceEvent	= if found then (Just (ReceiverEvent recLoc)) else Nothing			    
		    in return (found,deviceEvent,schedulerEvent)
	    where
		hasMenuHandlesMenu :: Id -> MenuHandles ps -> Bool
		hasMenuHandlesMenu menuId mHs@(MenuHandles {mMenus=mMenus}) =
			any (eqMenuId menuId) mMenus
			where
				eqMenuId :: Id -> MenuStateHandle ps -> Bool
				eqMenuId theId msH = theId == menuStateHandleGetMenuId msH


{-	filterToolbarEvent filters the OSEvents that can be handled by this menu device. -}

filterToolbarEvent :: OSDInfo -> OSEvent -> IOSt ps -> IO (Bool,Maybe [Int],Maybe DeviceEvent)

filterToolbarEvent osdInfo (CrossCallInfo{ccMsg=ccMsg,p2=toolbarPtr,p4=toolbarIndex}) ioState
	{-	ccWmBUTTONCLICKED is a menu event in case of a toolbar selection.  -}
	| ccMsg == ccWmBUTTONCLICKED =	
		if isToolbarEvent osdInfo toolbarPtr then
			return (True,Nothing,Just (ToolbarSelection toolbarIndex))
		else
			return (False,Nothing,Nothing)
	{-	ccWmGETTOOLBARTIPTEXT does not continue platform independent event handling, but returns the 
		String associated with the requested toolbar item.
	-}
	| ccMsg == ccWmGETTOOLBARTIPTEXT =
		if isToolbarEvent osdInfo toolbarPtr then
			let atts = ioStGetProcessAttributes ioState
			    (found,att)	= cselect isProcessToolbar undefined atts
			    maybe_tip = gettooltip toolbarIndex (getProcessToolbarAtt att)
			    
			    gettooltip :: Int -> [ToolbarItem ps] -> Maybe String
			    gettooltip i (item:items)
				| i==1 && isItem	= tip
				| otherwise		= gettooltip i' items
				where
					(isItem,i',tip)		= case item of
						ToolbarItem _ tip _	-> (True,i-1,tip)
						ToolbarSeparator	-> (False,i,Nothing)
		       	    gettooltip _ _ = Nothing
		       	in
			    if not found || isNothing maybe_tip then return (True,Nothing,Nothing)
			    else do
				 textptr <- newCString (fromJust maybe_tip)
				 return (True,Just [addr2int textptr],Nothing)
		else
			return (False,Nothing,Nothing)		     
	| otherwise = menuEventFatalError "filterToolbarEvent" "unmatched OSEvent"


{-	filterOSEvent filters the OSEvents that can be handled by this menu device.
		The Bool argument is True iff the parent process is visible and active.
-}
filterOSEvent :: OSEvent -> Bool -> (MenuHandles ps) -> IO (Bool,Maybe [Int],Maybe DeviceEvent)

{-	ccWmCOMMAND returns the selected menu item.
	This item is identified by:
	-	the top level menu Id,
	-	a possibly empty list of parent sub menus. This list is given by zero based indices starting from the top level menu.
	-	in the parent (sub) menu, the zero based index of the item.
	Only MenuItemHandle and SubMenuHandle elements increase the index; all other elements don't.
-}
filterOSEvent CrossCallInfo{ccMsg=ccMsg,p1=item,p2=mods} _ menus@(MenuHandles {mEnabled=mEnabled,mMenus=mHs})
    | ccMsg == ccWmCOMMAND =
	if not mEnabled then return (False,Nothing,Nothing)
	else do
		(found,deviceEvent) <- getSelectedMenuStateHandlesItem item mods mHs
		return (found,Nothing,deviceEvent)
    | otherwise =
	menuEventFatalError "filterOSEvent" "unmatched OSEvent"
    where
	getSelectedMenuStateHandlesItem :: Int -> Int -> [MenuStateHandle ps] -> IO (Bool,Maybe DeviceEvent)
	getSelectedMenuStateHandlesItem item mods [] =
	    return (False,Nothing)
	getSelectedMenuStateHandlesItem item mods (msH:msHs) = do
	    (found,menuEvent) <- getSelectedMenuStateHandleItem item mods msH
	    (if found then return (found,menuEvent)
	     else getSelectedMenuStateHandlesItem item mods msHs)
	    where
		getSelectedMenuStateHandleItem :: Int -> Int -> MenuStateHandle ps -> IO (Bool,Maybe DeviceEvent)
		getSelectedMenuStateHandleItem item mods msH@(MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH@(MenuHandle {mSelect=mSelect,mMenuId=mMenuId,mItems=mItems})}))
		    | not mSelect =
			return (False,Nothing)
		    | otherwise = do
			(found,menuEvent,_,_) <- getSelectedMenuElementHandlesItem item mMenuId mods [] 0 mItems
			return (found,menuEvent)
		    where
			getSelectedMenuElementHandlesItem :: Int -> Id -> Int -> [Int] -> Int -> [MenuElementHandle ls ps] -> IO (Bool,Maybe DeviceEvent,[Int],Int)
			getSelectedMenuElementHandlesItem item menuId mods parents zIndex [] =
			    return (False,Nothing,parents,zIndex)
			getSelectedMenuElementHandlesItem item menuId mods parents zIndex (itemH:itemHs) = do
			    (found,menuEvent,parents,zIndex) <- getSelectedMenuElementHandle item menuId mods parents zIndex itemH
			    (if found then return (found,menuEvent,parents,zIndex)
			     else getSelectedMenuElementHandlesItem item menuId mods parents zIndex itemHs)
			    where
				getSelectedMenuElementHandle :: Int -> Id -> Int -> [Int] -> Int -> MenuElementHandle ls ps -> IO (Bool,Maybe DeviceEvent,[Int],Int)
				
				getSelectedMenuElementHandle item menuId mods parents zIndex itemH@(MenuItemHandle {mOSMenuItem=mOSMenuItem,mItemId=mItemId})
					| item==mOSMenuItem =
						return (True,Just (MenuTraceEvent (MenuTraceInfo {mtId=menuId,mtParents=parents,mtItemNr=zIndex,mtModifiers=toModifiers mods})),parents,zIndex+1)
					| otherwise =
						return (False,Nothing,parents,zIndex+1)
				
				getSelectedMenuElementHandle item menuId mods parents zIndex itemH@(SubMenuHandle {mSubSelect=mSubSelect,mSubHandle=mSubHandle,mSubItems=mSubItems})
					| not mSubSelect =
						return (False,Nothing,parents,zIndex+1)
					| otherwise = do
						(found,menuEvent,parents1,_) <- getSelectedMenuElementHandlesItem item menuId mods (parents++[zIndex]) 0 mSubItems
						let parents2 = if found then parents1 else parents
						return (found,menuEvent,parents2,zIndex+1)

				getSelectedMenuElementHandle item menuId mods parents zIndex itemH@(RadioMenuHandle {mRadioSelect=mRadioSelect,mRadioItems=itemHs,mRadioIndex=mRadioIndex})
					| not mRadioSelect =
						return (False,Nothing,parents,zIndex+(length itemHs))
					| otherwise =
						getSelectedMenuElementHandlesItem item menuId mods parents zIndex itemHs						
				
				getSelectedMenuElementHandle item menuId mods parents zIndex (MenuListLSHandle itemHs) = do
					getSelectedMenuElementHandlesItem item menuId mods parents zIndex itemHs					
				
				getSelectedMenuElementHandle item menuId mods parents zIndex (MenuExtendLSHandle exLS itemHs) = do
					getSelectedMenuElementHandlesItem item menuId mods parents zIndex itemHs					
				
				getSelectedMenuElementHandle item menuId mods parents itemNr (MenuChangeLSHandle chLS itemHs) = do
					getSelectedMenuElementHandlesItem item menuId mods parents zIndex itemHs					
				
				getSelectedMenuElementHandle _ _ _ parents zIndex itemH =
					return (False,Nothing,parents,zIndex)


--	PA: this function is also defined identically in windowevent.
toModifiers :: Int -> Modifiers
toModifiers i
	= Modifiers
		{ shiftDown   = shifton
		, optionDown  = alton
		, commandDown = ctrlon
		, controlDown = ctrlon
		, altDown     = alton
		}
	where
		shifton = (i1 .&. (fromIntegral shiftBIT)) /= 0
		alton   = (i1 .&. (fromIntegral altBIT))   /= 0
		ctrlon  = (i1 .&. (fromIntegral ctrlBIT))  /= 0
		i1      = fromIntegral i :: Int32

--	isToolbarEvent checks whether the toolbar equals the OSDInfo toolbar.
isToolbarEvent :: OSDInfo -> OSWindowPtr -> Bool
isToolbarEvent osdInfo tPtr = 
	case getOSDInfoOSToolbar osdInfo of
		Nothing -> False
		Just toolbar -> toolbarPtr toolbar == tPtr	

menuHandlesGetMenuStateHandles :: MenuHandles ps -> ([MenuStateHandle ps],MenuHandles ps)
menuHandlesGetMenuStateHandles mHs@(MenuHandles {mMenus=mMenus})
	= (mMenus,mHs{mMenus=[]})
