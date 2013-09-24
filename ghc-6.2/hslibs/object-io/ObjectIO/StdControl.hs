--- StdControl specifies all control operations.

module StdControl ( getParentWindowId, controlSize
		  , openControls, openCompoundControls, openPopUpControlItems, closeControls
		  , closeAllControls, closePopUpControlItems, setControlPos, showControls
		  , showControl, hideControl, enableControls, enableControl, disableControls
		  , disableControl, markCheckControlItems, unmarkCheckControlItems
		  , setControlsMarkState, selectRadioControlItem, selectPopUpControlItem
		  , moveControlViewFrame, setControlViewDomain, setControlScrollFunction
		  , setControlTexts, setControlText, setEditControlCursor, setControlLooks
		  , setControlLook, setSliderStates, setSliderState, setSliderThumbs
		  , setSliderThumb, drawInControl, updateControl, getControlLayouts
		  , getControlLayout, getControlViewSizes, getControlViewSize
		  , getControlOuterSizes, getControlOuterSize, getControlSelectStates
		  , getControlSelectState, getControlShowStates, getControlShowState
		  , getControlTexts, getControlText, getControlNrLines, getControlNrLine
		  , getControlLooks, getControlLook, getControlMinimumSizes, getControlMinimumSize
		  , getControlResizes, getControlResize, getRadioControlItems, getRadioControlItem
		  , getRadioControlSelections, getRadioControlSelection, getCheckControlItems 
		  , getCheckControlItem, getCheckControlSelections, getCheckControlSelection
		  , getPopUpControlItems, getPopUpControlItem, getPopUpControlSelections
		  , getPopUpControlSelection, getSliderDirections, getSliderDirection
		  , getSliderStates, getSliderState, getControlViewFrames, getControlViewFrame 
		  , getControlViewDomains, getControlViewDomain, getControlScrollFunctions
		  , getControlScrollFunction
		  , module StdControlDef
		  , module StdControlClass
                  ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdControl specifies all control operations.
--	********************************************************************************


import CommonDef
import ControlAccess
import ControlInternal
import ControlLayout
import ControlValidate(disjointControlIds, controlIdsAreConsistent, getWElementControlIds)
import Id
import IOState
import OSSystem
import StdControlDef
import StdControlClass
import WindowAccess
import WindowUpdate(updateWindow)
import WindowClipState (forceValidWindowClipState)
import WindowControls
import OSWindow(osScrollbarsAreVisible)
import OSPicture(Draw)
import ReceiverId(unbindRIds)
import Monad(when)


stdControlFatalError :: String -> String -> x
stdControlFatalError function error
	= dumpFatalError function "StdControl" error

--	The function isOkControlId can be used to filter out the proper IdParent records.
isOkControlId :: SystemId -> (x,Maybe IdParent) -> (Bool,(x,Id))
isOkControlId ioId (x,Just (IdParent {idpIOId=idpIOId,idpDevice=idpDevice,idpId=idpId})) =
	(ioId==idpIOId && idpDevice==WindowDevice,(x,idpId))
isOkControlId _ _ =
	(False,undefined)

--	Two locally used functions that retrieve the parent Id(s).
getParentWindowId :: Id -> GUI ps (Maybe Id)
getParentWindowId id = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let maybeParent = getIdParent id idtable
	let (valid,(_,parentId)) = isOkControlId ioId ((),maybeParent)
	return (if valid then Just parentId else Nothing)

getParentWindowIds :: (a -> Id) -> [a] -> GUI ps [(a, Id)]
getParentWindowIds getId xs = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	let parentIds  = getIdParents (map getId xs) idtable
	return (filterMap (isOkControlId ioId) (zip xs parentIds))


mapWindow :: Id -> (forall ls . OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)) -> x -> GUI ps x
mapWindow windowId f x = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice);
	(if   not found
	 then stdControlFatalError "mapWindow" "WindowSystemState could not be retrieved from IOSt"
	 else 
		let
			windows              = windowSystemStateGetWindowHandles wDevice
			(found,wsH,windows1) = getWindowHandlesWindow (toWID windowId) windows
		in if not found then throwGUI ErrorUnknownObject
		   else do
				wMetrics <- accIOEnv ioStGetOSWindowMetrics
				(wsH1, x1) <- liftIO (ff f wMetrics wsH x)
				let windows2 = setWindowHandlesWindow wsH1 windows1
				appIOEnv (ioStSetDevice (WindowSystemState windows2))
				return x1)
	where
		ff :: (forall ls . OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)) -> OSWindowMetrics -> WindowStateHandle ps -> x -> IO (WindowStateHandle ps, x)
	  	ff f wMetrics (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) x = do
	  		(wH1,x1) <- f wMetrics wids wH x
	  		return (WindowStateHandle wids (Just wlsH{wlsHandle=wH1}),x1)


---
-- controlSize giving hMargins, vMargins and itemSpaces calculates the size of the given control.<BR>
-- example:<BR>
-- <code>controlSize (ButtonControl "Ok" []) True hMargins vMargins itemSpaces</code>
controlSize :: (Controls cdef) => cdef ls ps -> Bool -> Maybe (Int,Int) -> Maybe (Int,Int) -> Maybe (Int,Int) -> GUI ps Size
controlSize cdef isWindow hMargins vMargins itemSpaces
	= do {
		cs       <- controlToHandles cdef;
		wMetrics <- accIOEnv ioStGetOSWindowMetrics;
		let
			itemHs      = map controlStateToWElementHandle cs
			hMargins'   = case hMargins of
					Just (left,right) -> (max 0 left,max 0 right)
					_                 -> if isWindow then (0,0) else (osmHorMargin wMetrics,osmHorMargin wMetrics)
			vMargins'   = case vMargins of
					Just (top,bottom) -> (max 0 top,max 0 bottom)
					_                 -> if isWindow then (0,0) else (osmVerMargin wMetrics,osmVerMargin wMetrics)
			itemSpaces' = case itemSpaces of
					Just (hor,vert)   -> (max 0 hor,max 0 vert)
					_                 -> (osmHorItemSpace wMetrics,osmVerItemSpace wMetrics)
			domain      = viewDomainRange {corner1=zero}
		in liftIO (calcControlsSize wMetrics hMargins' vMargins' itemSpaces' zero zero [(domain,zero)] itemHs)
	  }


--- openControls adds controls to the indicated window.
openControls :: Controls cdef => Id -> ls -> cdef ls ps -> GUI ps ()
openControls wId ls newControls = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then throwGUI ErrorUnknownObject
	 else let
		windows = windowSystemStateGetWindowHandles wDevice
		(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
	      in if not found then throwGUI ErrorUnknownObject
	         else do
			cs <- controlToHandles newControls
			let newItemHs = map controlStateToWElementHandle cs
	  		let currentIds = getWindowStateHandleIds wsH
	  		(if not (disjointControlIds currentIds newItemHs) then throwGUI ErrorIdsInUse
			 else do
				rt <- ioStGetReceiverTable
				it <- ioStGetIdTable
				ioId <- accIOEnv ioStGetIOId
				(case controlIdsAreConsistent ioId wId newItemHs rt it of
					Nothing -> throwGUI ErrorIdsInUse
					Just (it,rt) -> do
						ioStSetIdTable it
						ioStSetReceiverTable rt	
						wMetrics <- accIOEnv ioStGetOSWindowMetrics
						wsH <- liftIO (opencontrols wMetrics ls newItemHs wsH)
						appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
						return ())))


---	getWindowStateHandleIds returns all Ids of the controls in this window.
-- 	This function is used by open(Compound)Controls.
getWindowStateHandleIds :: WindowStateHandle ps -> [Id]
getWindowStateHandleIds (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) =
	getWElementControlIds (whItems wH)
getWindowStateHandleIds _ = stdControlFatalError "getWindowStateHandleIds" "unexpected window placeholder argument"


---	openCompoundControls adds controls to the indicated CompoundControl
openCompoundControls :: Controls cdef => Id -> ls -> cdef ls ps -> GUI ps ()
openCompoundControls cId ls newControls = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> throwGUI ErrorUnknownObject
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			when (not found) (throwGUI ErrorUnknownObject)
			(let
				windows = windowSystemStateGetWindowHandles wDevice
				(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
			 in
				if not found then throwGUI ErrorUnknownObject
				else do
					cs <- controlToHandles newControls
					let newItemHs = map controlStateToWElementHandle cs
					let currentIds = getWindowStateHandleIds wsH
					(if not (disjointControlIds currentIds newItemHs) then throwGUI ErrorIdsInUse
					 else do
						rt <- ioStGetReceiverTable
						it <- ioStGetIdTable
						ioId <- accIOEnv ioStGetIOId
						(case controlIdsAreConsistent ioId wId newItemHs rt it of
							Nothing -> throwGUI ErrorIdsInUse
							Just (it,rt) -> do
								ioStSetIdTable it
								ioStSetReceiverTable rt	
								osdInfo <- accIOEnv ioStGetOSDInfo
								wMetrics <- accIOEnv ioStGetOSWindowMetrics
								(ok,wsH) <- liftIO (opencompoundcontrols osdInfo wMetrics cId ls newItemHs wsH)
								appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
								(if ok then return () else throwGUI ErrorUnknownObject)))))


--	openPopUpControlItems opens items to the PopUpControl of the indicated window/dialogue.

openPopUpControlItems :: Id -> Index -> [PopUpControlItem ps ps] -> GUI ps ()
openPopUpControlItems popUpId index [] = return ()
openPopUpControlItems popUpId index items = do
	maybeId	<- getParentWindowId popUpId
	(case maybeId of
		Nothing  -> return ()		
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			(if not found then return ()
			 else
				let
					wHs	= windowSystemStateGetWindowHandles wDevice
					(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
				in
					if not found
					then return ()
					else do		
							wsH <- liftIO (openpopupcontrolitems popUpId index items wsH)
							appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1)))))
	where
		openpopupcontrolitems :: Id -> Index -> [PopUpControlItem ps ps] -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		openpopupcontrolitems popUpId index items (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- openpopupitems popUpId index items (wPtr wids) wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		openpopupcontrolitems _ _ _ _ =
			stdControlFatalError "openPopUpControlItems" "unexpected window placeholder argument"


--	closeControls closes the controls in the indicated window.

closeControls :: Id -> [Id] -> Bool -> GUI ps ()
closeControls wId ids relayout = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else let
		windows = windowSystemStateGetWindowHandles wDevice
		(found,wsH,windows1) = getWindowHandlesWindow (toWID wId) windows
	      in
	     	if not found then return ()
		else do
			wMetrics <- accIOEnv ioStGetOSWindowMetrics
			(freeRIds,freeIds,disposeFun,wsH) <- liftIO (closecontrols wMetrics ids relayout wsH)
			unbindRIds freeRIds
			it <- ioStGetIdTable
			let (_,it1) = removeIdsFromIdTable (freeRIds++freeIds) it
			ioStSetIdTable it1
			f <- accIOEnv ioStGetInitIO
			appIOEnv (ioStSetInitIO (\ps -> f ps >>= \ps -> liftIO disposeFun >> return ps))
			appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1))))


--	closeAllControls closes all controls from the indicated window.

closeAllControls :: Id -> GUI ps ()
closeAllControls wId = do
	(found,wDevice) <- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return ()
	 else
		let
			wHs	= windowSystemStateGetWindowHandles wDevice
			(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
		in
			if not found then return ()
			else do
				(freeRIds,freeIds,disposeFun,wsH) <- liftIO (closeallcontrols wsH)
				unbindRIds freeRIds
				it <- ioStGetIdTable
				let (_,it1) = removeIdsFromIdTable (freeRIds++freeIds) it
				ioStSetIdTable it1
				f <- accIOEnv ioStGetInitIO
				appIOEnv (ioStSetInitIO (\ps -> f ps >>= \ps -> liftIO disposeFun >> return ps))
				appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1))))


--	closePopUpControlItems closes items from the indicated PopUpControl in the indicated window/dialogue.

closePopUpControlItems :: Id -> [Index] -> GUI ps ()
closePopUpControlItems popUpId [] = return ()
closePopUpControlItems popUpId indexs = do
	maybeId <- getParentWindowId popUpId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			(if not found then return ()
			 else
				let
					wHs	= windowSystemStateGetWindowHandles wDevice
					(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
				in
					if not found
					then return ()
					else do
						wsH <- liftIO (closepopupcontrolitems popUpId indexs wsH)
						appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs1)))))
	where
		closepopupcontrolitems :: Id -> [Index] -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		closepopupcontrolitems popUpId indexs wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			wH <- closepopupitems popUpId indexs (wPtr wids) wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		closepopupcontrolitems _ _ _ = do
			stdControlFatalError "closepopupcontrolitems" "unexpected window placeholder argument"


--	setControlPos changes the position of the indicated control.

setControlPos :: Id -> [(Id,ItemPos)] -> GUI ps Bool
setControlPos wId newPoss = do
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(if not found then return False
	 else
		let
			wHs		 = windowSystemStateGetWindowHandles wDevice
			(found,wsH,wHs1) = getWindowHandlesWindow (toWID wId) wHs
		in if not found then return False
		   else do
				wMetrics <- accIOEnv ioStGetOSWindowMetrics
				(ok,wsH) <- liftIO (setcontrolpositions wMetrics newPoss wsH)
				let wHs2 = setWindowHandlesWindow wsH wHs1
				appIOEnv (ioStSetDevice (WindowSystemState wHs2))
				return ok)


--	Show/Hide controls.

showControls :: [Id] -> GUI ps ()
showControls = setControlsShowState True

showControl :: Id -> GUI ps ()
showControl id = showControls [id]

hideControls :: [Id] -> GUI ps ()
hideControls = setControlsShowState False

hideControl :: Id -> GUI ps ()
hideControl id = hideControls [id]

setControlsShowState :: Bool -> [Id] -> GUI ps ()
setControlsShowState show ids = do
	cIds_wIds <- getParentWindowIds id ids
	sequence_ [mapWindow wId (setControlsShowState cIds) () | (cIds,wId)<-gather cIds_wIds]
	where
		setControlsShowState :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlsShowState ids wMetrics wids wH s = do
			wH <- setcontrolsshowstate ids show wMetrics wids wH
			wH <- forceValidWindowClipState wMetrics True (wPtr wids) wH			
			return (wH, s)

--	Enabling/Disabling of controls.

enableControls :: [Id] -> GUI ps ()
enableControls ids = do
	cIds_wIds <- getParentWindowIds id ids
	sequence_ [mapWindow wId (enableControls cIds) () | (cIds,wId)<-gather cIds_wIds]
	where
		enableControls :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		enableControls ids wMetrics wids wH s = do
			wH <- enablecontrols ids False wMetrics (wPtr wids) wH
			return (wH, s)

enableControl :: Id -> GUI ps ()
enableControl id = enableControls [id]

disableControls :: [Id] -> GUI ps ()
disableControls ids = do
	cIds_wIds <- getParentWindowIds id ids
	sequence_ [mapWindow wId (disableControls cIds) () | (cIds,wId)<-gather cIds_wIds]
	where
		disableControls :: [Id] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		disableControls ids wMetrics wids wH s = do
			wH <- disablecontrols ids False wMetrics (wPtr wids) wH
			return (wH, s)

disableControl :: Id -> GUI ps ()
disableControl id = disableControls [id]


--	Marking/Unmarking of check controls.

markCheckControlItems :: Id -> [Index] -> GUI ps ()
markCheckControlItems cId indexs = setControlsMarkState Mark cId indexs

unmarkCheckControlItems :: Id -> [Index] -> GUI ps ()
unmarkCheckControlItems cId indexs = setControlsMarkState NoMark cId indexs

setControlsMarkState :: MarkState -> Id -> [Index] -> GUI ps ()
setControlsMarkState mark cId [] = return ()
setControlsMarkState mark cId indexs = do	
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setControlsMarkState mark cId indexs) ())
	where
		setControlsMarkState :: MarkState -> Id -> [Index] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlsMarkState mark id indexs wMetrics wids wH s = do
			wH <- setcontrolsmarkstate id mark indexs wMetrics (wPtr wids) wH
			return (wH,s)


--	Selecting/Unselecting a radio control.

selectRadioControlItem :: Id -> Index -> GUI ps ()
selectRadioControlItem cId index = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (selectRadioControlItem cId index) ())
	where
		selectRadioControlItem :: Id -> Index -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		selectRadioControlItem id index wMetrics wids wH s = do
			wH <- selectradiocontrol id index wMetrics (wPtr wids) wH
			return (wH,s)


--	Select a pop up menu item.

selectPopUpControlItem :: Id -> Index -> GUI ps ()
selectPopUpControlItem cId index = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (selectPopUpControlItem cId index) ())
	where
		selectPopUpControlItem :: Id -> Index -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		selectPopUpControlItem id index wMetrics wids wH s = do
			wH <- selectpopupitem id index wMetrics (wPtr wids) wH
			return (wH,s)

--	Move the orientation of a CompoundControl.

moveControlViewFrame :: Id -> Vector2 -> GUI ps ()
moveControlViewFrame cId v = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (moveControlViewFrame cId v) ())
	where
		moveControlViewFrame :: Id -> Vector2 -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		moveControlViewFrame id v wMetrics wids wH s = do
			wH <- movecontrolviewframe id v wMetrics wids wH		
			return (wH, s)

--	Set a new view domain of a CompoundControl.

setControlViewDomain :: Id -> ViewDomain -> GUI ps ()
setControlViewDomain cId newDomain = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setControlViewDomain cId newDomain) ())
	where
		setControlViewDomain :: Id -> ViewDomain -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlViewDomain id newDomain wMetrics wids wH s = do
			wH <- setcontrolviewdomain id newDomain wMetrics wids wH
			return (wH, s)

--	Set the ScrollFunction of a CompoundControl.

setControlScrollFunction :: Id -> Direction -> ScrollFunction -> GUI ps ()
setControlScrollFunction cId direction scrollFun = do
	maybeId <- getParentWindowId cId
	(case maybeId of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setControlScrollFunction cId direction scrollFun) ())
	where
		setControlScrollFunction :: Id -> Direction -> ScrollFunction -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> x -> IO (WindowHandle ls ps, x)
		setControlScrollFunction id direction scrollFun wMetrics wids wH s = do
			wH <- setcontrolscrollfun id direction scrollFun wH
			return (wH, s)


--	Change the text of (Text/Edit/Button)Control.

setControlTexts :: [(Id,String)] -> GUI ps ()
setControlTexts cid_texts = do
	cid_texts_wIds <- getParentWindowIds fst cid_texts
	sequence_ [mapWindow wId (setControlTexts' cid_texts) () | (cid_texts,wId)<-gather cid_texts_wIds]
	where
		setControlTexts' :: [(Id,String)] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setControlTexts' texts wMetrics wids wH s = do
			wH1 <- setcontroltexts texts wMetrics (wPtr wids) wH
			return (wH1, s)

setControlText :: Id -> String -> GUI ps ()
setControlText id text = setControlTexts [(id,text)]


--	Set the cursor position of an EditControl.

setEditControlCursor :: Id -> Int -> GUI ps ()
setEditControlCursor cId pos = do
	maybeParent <- getParentWindowId cId
	(case maybeParent of
		Nothing  -> return ()
		Just wId -> mapWindow wId (setEditControlCursor cId pos) ())
	where
		setEditControlCursor :: Id -> Int -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setEditControlCursor id pos wMetrics wids wH s = do
			wH <- seteditcontrolcursor id pos wMetrics (wPtr wids) wH
			return (wH, s)


{-	Change the Look of the corresponding (Custom(Button)/Compound)Controls and redraw
	only if the first Boolean is True.
-}

setControlLooks :: [(Id,Bool,(Bool,Look))] -> GUI ps ()
setControlLooks cid_looks = do		
	cid_looks_wIds <- getParentWindowIds (\(x,y,z) -> x) cid_looks
	sequence_ [mapWindow wId (setControlLooks cid_looks) () |	(cid_looks,wId)<-gather cid_looks_wIds]
	where
		setControlLooks :: [(Id,Bool,(Bool,Look))] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setControlLooks looks wMetrics wids wH s = do
			wH <- setcontrolslook looks wMetrics (wPtr wids) wH
			return (wH, s)

setControlLook :: Id -> Bool -> (Bool,Look) -> GUI ps ()
setControlLook id redraw newlook = setControlLooks [(id,redraw,newlook)]


--	Change the SliderState and redraw the settings of the SliderControls.

setSliderStates :: [(Id,IdFun SliderState)] -> GUI ps ()
setSliderStates cid_fs = do		
	cid_fs_wIds <- getParentWindowIds fst cid_fs
	sequence_ [mapWindow wId (setSliderStates cid_fs) () | (cid_fs,wId)<-gather cid_fs_wIds]
	where
		setSliderStates :: [(Id,IdFun SliderState)] -> OSWindowMetrics -> WIDS -> WindowHandle ls ps -> s -> IO (WindowHandle ls ps, s)
		setSliderStates id_fs wMetrics wids wH s = do
			wH <- setsliderstates id_fs wMetrics (wPtr wids) wH
			return (wH, s)

setSliderState :: Id -> IdFun SliderState -> GUI ps ()
setSliderState id fun = setSliderStates [(id,fun)]


--	Change the thumb value of the SliderState of a SliderControl. 

setSliderThumbs :: [(Id,Int)] -> GUI ps ()
setSliderThumbs cid_thumbs =
	setSliderStates (map (\(cid,thumb)->(cid,\state->state{sliderThumb=thumb})) cid_thumbs)

setSliderThumb :: Id -> Int -> GUI ps ()
setSliderThumb id thumb = setSliderThumbs [(id,thumb)]


--	Draw in a {Custom(Button)|Compound}Control.

drawInControl :: Id -> Draw x -> GUI ps (Maybe x)
drawInControl cId drawfun = do
	maybeParent <- getParentWindowId cId
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(case maybeParent of		
		Just wId | found ->
			let
				windows 	= windowSystemStateGetWindowHandles wDevice
				(_,wsH,windows1)= getWindowHandlesWindow (toWID wId) windows
				wids 		= getWindowStateHandleWIDS wsH
			in do
				wMetrics <- accIOEnv ioStGetOSWindowMetrics
				(maybe_result,wsH) <- liftIO (drawInControl cId drawfun wMetrics (wPtr wids) wsH)
				let windows2 = setWindowHandlesWindow wsH windows1
				appIOEnv (ioStSetDevice (WindowSystemState windows2))
				return maybe_result
		_  -> return Nothing)
	where
		drawInControl :: Id -> Draw x -> OSWindowMetrics -> OSWindowPtr -> WindowStateHandle ps -> IO (Maybe x,WindowStateHandle ps)
		drawInControl controlId drawfun wMetrics wPtr (WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH}))) = do
			(maybe_result,wH) <- drawincontrol controlId drawfun wMetrics wPtr wH
			return (maybe_result,WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
		drawInControl _ _ _ _ _ = stdControlFatalError "drawInControl" "unexpected window placeholder argument"

--	Update a selection of a (Compound/Custom(Button))Control:

updateControl :: Id -> Maybe ViewFrame -> GUI ps ()
updateControl cId maybeViewFrame = do
	maybeParent <- getParentWindowId cId
	(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
	(case maybeParent of
		Just wId | found ->
			let
				windows 	= windowSystemStateGetWindowHandles wDevice
				(_,wsH,windows1)= getWindowHandlesWindow (toWID wId) windows
				wKind 		= getWindowStateHandleWindowKind wsH
			in
				if wKind /= IsWindow
				then appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
				else do
					wMetrics <- accIOEnv ioStGetOSWindowMetrics
					wsH <- liftIO (updateControlBackground wMetrics wKind cId maybeViewFrame wsH)
					appIOEnv (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows1)))
		Nothing -> return ())
	where
		updateControlBackground :: OSWindowMetrics -> WindowKind -> Id -> Maybe ViewFrame -> WindowStateHandle ps -> IO (WindowStateHandle ps)
		updateControlBackground wMetrics wKind cId maybeViewFrame wsH@(WindowStateHandle wids (Just wlsH@(WindowLSHandle {wlsHandle=wH@(WindowHandle {whSize=whSize,whItems=itemHs})}))) = do
			let (Just updInfo) = getWElementHandlesUpdateInfo wMetrics cId contentRect itemHs
			wH <- updateWindow wMetrics updInfo wH
			return (WindowStateHandle wids (Just wlsH{wlsHandle=wH}))
			where
				info = whWindowInfo wH
				(domainRect,hasScrolls)	= case wKind of
					IsWindow -> (windowDomain info,(isJust (windowHScroll info),isJust (windowVScroll info)))
					_        -> (sizeToRect whSize,(False,False))
				visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
				contentRect	= getWindowContentRect wMetrics visScrolls (sizeToRect whSize)

				getWElementHandlesUpdateInfo :: OSWindowMetrics -> Id -> Rect -> [WElementHandle ls ps] -> Maybe UpdateInfo
				getWElementHandlesUpdateInfo wMetrics cId clipRect [] = Nothing
				getWElementHandlesUpdateInfo wMetrics cId clipRect (itemH:itemHs) =
					let res = getWElementHandleUpdateInfo wMetrics cId clipRect itemH
					in  if isJust res then res
					    else getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs
					where
						getWElementHandleUpdateInfo :: OSWindowMetrics -> Id -> Rect -> WElementHandle ls ps -> Maybe UpdateInfo
						getWElementHandleUpdateInfo wMetrics cId clipRect itemH@(WItemHandle {wItemKind=itemKind,wItemPos=itemPos,wItemSize=itemSize})
							| wItemId itemH /= Just cId =
								if not (isRecursiveControl itemKind) then Nothing
								else getWElementHandlesUpdateInfo wMetrics cId visRect (wItems itemH)
							| itemKind `elem` [IsCompoundControl,IsCustomControl,IsCustomButtonControl] = Just updInfo
							| otherwise = Nothing
							where
								itemRect	= posSizeToRect itemPos itemSize
								itemInfo	= wItemInfo itemH
								compoundInfo	= getWItemCompoundInfo itemInfo
								origin		= if itemKind==IsCompoundControl
										  then compoundOrigin compoundInfo
										  else zero
								domain		= compoundDomain compoundInfo
								hasScrolls	= (isJust (compoundHScroll compoundInfo),isJust (compoundVScroll compoundInfo))
								visScrolls	= osScrollbarsAreVisible wMetrics domain (toTuple itemSize) hasScrolls
								contentRect	= if itemKind==IsCompoundControl
										  then getCompoundContentRect wMetrics visScrolls itemRect
										  else itemRect
								visRect		= intersectRects contentRect clipRect
								updArea		= case maybeViewFrame of
									Nothing	  -> visRect
									Just rect -> intersectRects (rectangleToRect (addVector (toVector itemPos)
																(subVector (toVector origin) rect)
														     )
												    ) visRect
								updInfo	= UpdateInfo
									{ updWIDS 	= wids
									, updWindowArea	= zero
									, updControls	= [ControlUpdateInfo
												{ cuItemNr	= wItemNr itemH
												, cuItemPtr	= wItemPtr itemH
												, cuArea		= updArea
												}]
									, updGContext	= Nothing
									}
						getWElementHandleUpdateInfo wMetrics cId clipRect (WListLSHandle itemHs) =
							getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs

						getWElementHandleUpdateInfo wMetrics cId clipRect (WExtendLSHandle exLS itemHs) =
							getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs

						getWElementHandleUpdateInfo wMetrics cId clipRect (WChangeLSHandle chLS itemHs) =
							getWElementHandlesUpdateInfo wMetrics cId clipRect itemHs

		updateControlBackground _ _ _ _ _ = stdControlFatalError "updateControl" "unexpected window placeholder argument"


getControlLayouts :: [Id] -> GUI ps [(Bool,(Maybe ItemPos,Vector2))]
getControlLayouts ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolslayouts ids)) [(id,False,(Nothing,zero)) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlLayout :: Id -> GUI ps (Bool,(Maybe ItemPos,Vector2))
getControlLayout id = fmap head (getControlLayouts [id])


getControlViewSizes :: [Id] -> GUI ps [(Bool,Size)]
getControlViewSizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsviewsizes ids)) [(id,False,zero) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlViewSize :: Id -> GUI ps (Bool,Size)
getControlViewSize id = fmap head (getControlViewSizes [id])


getControlOuterSizes :: [Id] -> GUI ps [(Bool,Size)]
getControlOuterSizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsoutersizes ids)) [(id,False,zero) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)	

getControlOuterSize :: Id -> GUI ps (Bool,Size)
getControlOuterSize id = fmap head (getControlOuterSizes [id])


getControlSelectStates :: [Id] -> GUI ps [(Bool,SelectState)]
getControlSelectStates ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsselects ids)) [(id,False,Able) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlSelectState :: Id -> GUI ps (Bool,SelectState)
getControlSelectState id = fmap head (getControlSelectStates [id])


getControlShowStates :: [Id] -> GUI ps [(Bool,Bool)]
getControlShowStates ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsshowstates ids)) [(id,False,False) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlShowState :: Id -> GUI ps (Bool,Bool)
getControlShowState id = fmap head (getControlShowStates [id])

--	Access operations on Windows:

getControlTexts :: [Id] -> GUI ps [(Bool,Maybe String)]
getControlTexts ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolstexts ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlText :: Id -> GUI ps (Bool,Maybe String)
getControlText id = fmap head (getControlTexts [id])


getControlNrLines :: [Id] -> GUI ps [(Bool,Maybe NrLines)]
getControlNrLines ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsnrlines ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlNrLine :: Id -> GUI ps (Bool,Maybe NrLines)
getControlNrLine id = fmap head (getControlNrLines [id])


getControlLooks :: [Id] -> GUI ps [(Bool,Maybe (Bool,Look))]
getControlLooks ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolslooks ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlLook :: Id -> GUI ps (Bool,Maybe (Bool,Look))
getControlLook id = fmap head (getControlLooks [id])


getControlMinimumSizes :: [Id] -> GUI ps [(Bool,Maybe Size)]
getControlMinimumSizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsminsizes ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlMinimumSize :: Id -> GUI ps (Bool,Maybe Size)
getControlMinimumSize id = fmap head (getControlMinimumSizes [id])


getControlResizes :: [Id] -> GUI ps [(Bool,Maybe ControlResizeFunction)]
getControlResizes ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsresizes ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlResize :: Id -> GUI ps (Bool,Maybe ControlResizeFunction)
getControlResize id = fmap head (getControlResizes [id])


getRadioControlItems :: [Id] -> GUI ps [(Bool,Maybe [String])]
getRadioControlItems ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getradioitems ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getRadioControlItem :: Id -> GUI ps (Bool,Maybe [String])
getRadioControlItem id = fmap head (getRadioControlItems [id])


getRadioControlSelections :: [Id] -> GUI ps [(Bool,Maybe Index)]
getRadioControlSelections ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getradiocontrolsmarks ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getRadioControlSelection :: Id -> GUI ps (Bool,Maybe Index)
getRadioControlSelection id = fmap head (getRadioControlSelections [id])


getCheckControlItems :: [Id] -> GUI ps [(Bool,Maybe [String])]
getCheckControlItems ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcheckitems ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getCheckControlItem :: Id -> GUI ps (Bool,Maybe [String])
getCheckControlItem id = fmap head (getCheckControlItems [id])


getCheckControlSelections :: [Id] -> GUI ps [(Bool,Maybe [Index])]
getCheckControlSelections ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcheckcontrolsmarks ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getCheckControlSelection :: Id -> GUI ps (Bool,Maybe [Index])
getCheckControlSelection id = fmap head (getCheckControlSelections [id])


getPopUpControlItems :: [Id] -> GUI ps [(Bool,Maybe [String])]
getPopUpControlItems ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getpopupitems ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getPopUpControlItem :: Id -> GUI ps (Bool,Maybe [String])
getPopUpControlItem id = fmap head (getPopUpControlItems [id])


getPopUpControlSelections :: [Id] -> GUI ps [(Bool,Maybe Index)]
getPopUpControlSelections ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getselectedpopupitems ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getPopUpControlSelection :: Id -> GUI ps (Bool,Maybe Index)
getPopUpControlSelection id = fmap head (getPopUpControlSelections [id])


getSliderDirections :: [Id] -> GUI ps [(Bool,Maybe Direction)]
getSliderDirections ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getslidersdirections ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getSliderDirection :: Id -> GUI ps (Bool,Maybe Direction)
getSliderDirection id = fmap head (getSliderDirections [id])

getSliderStates :: [Id] -> GUI ps [(Bool,Maybe SliderState)]
getSliderStates ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getslidersstates ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getSliderState :: Id -> GUI ps (Bool,Maybe SliderState)
getSliderState id = fmap head (getSliderStates [id])


getControlViewFrames :: [Id] -> GUI ps [(Bool,Maybe ViewFrame)]
getControlViewFrames ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsframes ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlViewFrame :: Id -> GUI ps (Bool,Maybe ViewFrame)
getControlViewFrame id = fmap head (getControlViewFrames [id])


getControlViewDomains :: [Id] -> GUI ps [(Bool,Maybe ViewDomain)]
getControlViewDomains ids = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getcontrolsdomains ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlViewDomain :: Id -> GUI ps (Bool,Maybe ViewDomain)
getControlViewDomain id = fmap head (getControlViewDomains [id])


getControlScrollFunctions :: [Id] -> GUI ps [(Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))]
getControlScrollFunctions ids  = do
	cid_pids <- getParentWindowIds id ids
	tbl <- foldrM (\(ids, wId) -> mapWindow wId (getscrollfunctions ids)) [(id,False,Nothing) | (id,pid)<-cid_pids] (gather cid_pids)
	return (map snd3thd3 tbl)

getControlScrollFunction :: Id -> GUI ps (Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))
getControlScrollFunction id = fmap head (getControlScrollFunctions [id])