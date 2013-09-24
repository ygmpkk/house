module StdMenu( Menus(..)
	      ,	closeMenu
	      , enableMenuSystem
	      , disableMenuSystem
	      , enableMenus
	      , disableMenus
	      , getMenuSelectState
	      , openMenuElements
	      , openSubMenuElements
	      , openRadioMenuItems
	      , closeMenuElements
	      , closeMenuIndexElements
	      , closeSubMenuIndexElements
	      , closeRadioMenuIndexElements
	      , getMenus
	      , getMenuPos
	      , setMenuTitle
	      , getMenuTitle 
	      , module StdMenuDef) where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	********************************************************************************

import	CommonDef
import	IOState
import	Id
import	StdId(openId)
import  OSMenu(osTrackPopUpMenu, osEnableMenu, osDisableMenu)
import	MenuCreate
import	MenuDevice
import	qualified MenuInternal as I
import	MenuHandle
import	MenuItems
import  StdMenuElementClass
import  StdMenuDef
import	DeviceSystemState(windowSystemStateGetWindowHandles)
import	MenuAccess(menuStateHandleGetMenuId, menuStateHandleGetSelect, menuStateHandleGetTitle, menuStateHandleGetHandle)
import	MenuDefAccess(menuDefGetMenuId)
import	MenuEvent(menuHandlesGetMenuStateHandles)
import	DeviceSystemState(menuSystemStateGetMenuHandles)
import	WindowAccess(getWindowHandlesActiveModalDialog)
import  Monad(when)

stdMenuFatalError :: String -> String -> x
stdMenuFatalError function error =
	dumpFatalError function "StdMenu" error


--	General rules to access MenuHandles:


accessMenuHandles :: Id -> (MenuStateHandle ps -> x) -> GUI ps (Maybe x)
accessMenuHandles id f = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return Nothing
	 else do return (accessmenuhandles id f (mMenus (menuSystemStateGetMenuHandles mDevice))))
	where
		accessmenuhandles :: Id -> (MenuStateHandle ps -> x) -> [MenuStateHandle ps] -> Maybe x
		accessmenuhandles id f [] = Nothing
		accessmenuhandles id f (mH:mHs)
			| id==menuStateHandleGetMenuId mH = Just (f mH)
			| otherwise = accessmenuhandles id f mHs


--	Opening a menu for an interactive process.

class Menus mdef where
	openMenu :: ls -> mdef ls ps -> ps -> GUI ps ps

instance MenuElements m => Menus (Menu m) where	
	openMenu ls mDef ps = do
		ps <- dOpen menuFunctions ps
		isZero <- checkZeroMenuBound
		(if isZero then throwGUI ErrorViolateDI
		 else do
			optMenuId <- validateMenuId (menuDefGetMenuId mDef)
			(case optMenuId of
				Just menuId | menuId /= windowMenuId -> openMenu' menuId ls mDef ps
				_ 			     	     -> throwGUI ErrorIdsInUse))
		where
			checkZeroMenuBound :: GUI ps Bool
			checkZeroMenuBound = do
				(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
				(if not found
				 then stdMenuFatalError "openMenu (Menu)" "could not retrieve MenuSystemState from IOSt"
				 else return (zeroBound (mNrMenuBound (menuSystemStateGetMenuHandles mDevice))))

			validateMenuId :: Maybe Id -> GUI ps (Maybe Id)
			validateMenuId Nothing = do
				mId <- openId
				return (Just mId)
			validateMenuId (Just id) = do
				idtable <- ioStGetIdTable
				return (if memberIdTable id idtable then Nothing else Just id)

instance PopUpMenuElements m => Menus (PopUpMenu m) where
	openMenu ls mDef ps = do
		osdInfo <- accIOEnv ioStGetOSDInfo
		(if getOSDInfoDocumentInterface osdInfo==NDI
		 then throwGUI ErrorViolateDI
		 else case getOSDInfoOSMenuBar osdInfo of
			Nothing -> stdMenuFatalError "openMenu (PopUpMen)" "OSMenuBar could not be retrieved from OSDInfo"
			Just osMenuBar -> do
				ps <- dOpen menuFunctions ps
				(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
				(if not found then throwGUI ErrorUnknownObject
				 else let 
					 mHs  = menuSystemStateGetMenuHandles mDevice
		  			 mHs1 = closePopUpMenu mHs
		  		      in do
		  		     		it <- ioStGetIdTable
						rt <- ioStGetReceiverTable
						ioid <- accIOEnv ioStGetIOId
						(ok,mHs2,rt,it) <- createPopUpMenu ioid ls mDef mHs1 rt it osMenuBar	 	  
						ioStSetReceiverTable rt
						ioStSetIdTable it
						appIOEnv (ioStSetDevice (MenuSystemState mHs2))		
						(if ok then handlePopUpMenu ps
						 else throwGUI ErrorIdsInUse)))
		where
		--	handlePopUpMenu opens the pop up menu.
			handlePopUpMenu :: ps -> GUI ps ps
			handlePopUpMenu ps = do
				osdInfo <- accIOEnv ioStGetOSDInfo
				let framePtr = case getOSDInfoOSInfo osdInfo of
					Just info -> osFrame info
					Nothing   -> stdMenuFatalError "openMenu (PopUpMenu)" "incorrect OSDInfo retrieved"
				(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
				when (not found) (stdMenuFatalError "openMenu (PopUpMenu)" "could not retrieve MenuSystemState from IOSt")
				let mHs	= menuSystemStateGetMenuHandles mDevice
				let ((popUpMenu:menus),mHs1) = menuHandlesGetMenuStateHandles mHs				
				let popUpId = menuStateHandleGetMenuId popUpMenu
				let mPtr = menuStateHandleGetHandle popUpMenu
				ok <- liftIO (osTrackPopUpMenu mPtr framePtr)
				(if not ok
				 then do
					appIOEnv (ioStSetDevice (MenuSystemState mHs1{mMenus=menus,mPopUpId=Just popUpId}))
					throwGUI (OtherError "PopUpMenu tracking error")
				 else do
					appIOEnv (ioStSetDevice (MenuSystemState mHs1{mMenus=popUpMenu:menus}))
					return ps)



--	Closing a menu.

closeMenu :: Id -> GUI ps ()
closeMenu id
	| id==windowMenuId = return ()
	| otherwise	   = I.closeMenu id

checkIdParent :: IdParent -> SystemId -> Bool
checkIdParent parent ioId = idpDevice parent==MenuDevice && idpIOId parent==ioId


--	Enabling and Disabling of the MenuSystem:

enableMenuSystem :: GUI ps ()
enableMenuSystem = do
	isModal <- hasModalDialog
	di <- accIOEnv ioStGetDocumentInterface
	(if isModal || di==NDI then return ()
	 else I.changeMenuSystemState True (enableMenuSystem' di))
	where
		hasModalDialog :: GUI ps Bool
		hasModalDialog = do
			(found,wDevice)	<- accIOEnv (ioStGetDevice WindowDevice)
			let windows	= windowSystemStateGetWindowHandles wDevice
			return (found && isJust (getWindowHandlesActiveModalDialog windows))

		enableMenuSystem' :: DocumentInterface -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
		enableMenuSystem' di osMenuBar menus@(MenuHandles {mEnabled=mEnabled,mMenus=mMenus})
			| mEnabled = return menus
			| otherwise = do
				let nrMenus = length mMenus
				enableMenus (if di==MDI then nrMenus+1 else nrMenus-1) osMenuBar
				return (menus{mEnabled=systemAble})
			where
				enableMenus :: Int -> OSMenuBar -> IO ()
				enableMenus i osMenuBar
					| i<0		= return ()
					| otherwise	= do
						osEnableMenu i osMenuBar
						enableMenus (i-1) osMenuBar
						
						
disableMenuSystem :: GUI ps ()
disableMenuSystem = do	
	di <- accIOEnv ioStGetDocumentInterface
	(if di==NDI then return ()
	 else I.changeMenuSystemState True (disableMenuSystem' di))
	where
		disableMenuSystem' :: DocumentInterface -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
		disableMenuSystem' di osMenuBar menus@(MenuHandles {mEnabled=mEnabled,mMenus=mMenus})
			| mEnabled = return menus
			| otherwise = do
				let nrMenus = length mMenus
				disableMenus (if di==MDI then nrMenus+1 else nrMenus-1) osMenuBar
				return (menus{mEnabled=systemUnable})
			where
				disableMenus :: Int -> OSMenuBar -> IO ()
				disableMenus i osMenuBar
					| i<0		= return ()
					| otherwise	= do
						osDisableMenu i osMenuBar
						disableMenus (i-1) osMenuBar

--	Enabling and Disabling of Menus:

enableMenus :: [Id] -> GUI ps ()
enableMenus ids =
	let ids1 = filter ((/=) windowMenuId) ids
	in if null ids1 then return ()
	   else I.enableMenus ids1

disableMenus :: [Id] -> GUI ps ()
disableMenus ids =
	let ids1 = filter ((/=) windowMenuId) ids
	in if null ids1 then return ()
	   else I.disableMenus ids1	


--	Get the SelectState of a menu: 

getMenuSelectState :: Id -> GUI ps (Maybe SelectState)
getMenuSelectState id = do
	optSelect <- accessMenuHandles id menuStateHandleGetSelect
	return (fmap (\sel -> if sel then Able else Unable) optSelect)


{-	Adding menu elements to (sub/radio)menus:
		Items in a (sub/radio)menu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front
		Open with a position higher than the number of items adds the new elements to
		the end.
		Open an item on a position adds the item AFTER the item on that position.
-}
openMenuElements :: MenuElements m => Id -> Index -> ls -> m ls ps -> GUI ps ()
openMenuElements mId pos ls new = do
	it   <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case getIdParent mId it of
		Just parent | checkIdParent parent ioId && idpId parent == mId -> do
			(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
			when (not found) (throwGUI ErrorUnknownObject)
			osdInfo <- accIOEnv ioStGetOSDInfo
			(case getOSDInfoOSMenuBar osdInfo of
				Nothing -> stdMenuFatalError "openMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
				Just osMenuBar -> do
					rt <- ioStGetReceiverTable
					let menus = menuSystemStateGetMenuHandles mDevice
					(rt,it,menus) <- addMenusItems (mId,Nothing) (max 0 pos) ls new ioId rt it menus osMenuBar					
					appIOEnv (ioStSetDevice (MenuSystemState menus))
					ioStSetIdTable it
					ioStSetReceiverTable rt
					return ())
	  	_ -> throwGUI ErrorUnknownObject)
	
	
openSubMenuElements :: MenuElements m => Id -> Index -> ls -> m ls ps -> GUI ps ()
openSubMenuElements sId pos ls new = do
	it   <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case getIdParent sId it of
		Just parent | checkIdParent parent ioId -> do
			(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
			when (not found) (throwGUI ErrorUnknownObject)
			osdInfo <- accIOEnv ioStGetOSDInfo
			(case getOSDInfoOSMenuBar osdInfo of
				Nothing -> stdMenuFatalError "openSubMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
				Just osMenuBar -> do
					rt <- ioStGetReceiverTable
					let menus = menuSystemStateGetMenuHandles mDevice					
					(rt,it,menus) <- addMenusItems (idpId parent,Just sId) (max 0 pos) ls new ioId rt it menus osMenuBar
					appIOEnv (ioStSetDevice (MenuSystemState menus))
					ioStSetIdTable it
					ioStSetReceiverTable rt
					return ())
		_ -> throwGUI ErrorUnknownObject)
	


openRadioMenuItems :: Id -> Index -> [MenuRadioItem ps ps] -> GUI ps ()
openRadioMenuItems rId pos radioItems = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case getIdParent rId idtable of
		Just parent | checkIdParent parent ioId ->
			if null radioItems then return ()
			else
				let radioIds = filterMap (\(_,maybeId,_,_)->(isJust maybeId,fromJust maybeId)) radioItems
				in if not (okMembersIdTable radioIds idtable)
				   then throwGUI ErrorIdsInUse
				   else do
					   let mId	= idpId parent
					   I.changeMenuSystemState True (addMenuRadioItems (mId,rId) (max 0 pos) radioItems)
					   let (_, idtable1) = addIdsToIdTable (map (\id->(id,IdParent{idpIOId=ioId,idpDevice=MenuDevice,idpId=mId})) radioIds) idtable
					   ioStSetIdTable idtable1
		_ 	-> throwGUI ErrorUnknownObject)

--	Removing menu elements from (sub/radio)menus:

closeMenuElements :: Id -> [Id] -> GUI ps ()
closeMenuElements mId ids =
	let ids1 = filter (\id->not (isSpecialId id)) ids
	in if null ids1 then return ()
	   else I.closeMenuElements mId ids1

--	Removing menu elements from (sub/radio)menus by index (counting from 1):

closeMenuIndexElements :: Id -> [Index] -> GUI ps ()
closeMenuIndexElements mId indices = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case getIdParent mId idtable of
		Just parent | checkIdParent parent ioId ->
			I.closeMenuIndexElements I.notRemoveSpecialMenuElements False ioId (mId,Nothing) indices
		_ -> return ())


closeSubMenuIndexElements :: Id -> [Index] -> GUI ps ()
closeSubMenuIndexElements sId indices = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case getIdParent sId idtable of
		Just parent | checkIdParent parent ioId ->
			I.closeMenuIndexElements I.notRemoveSpecialMenuElements False ioId (idpId parent,Just sId) indices
		_ -> return ())


closeRadioMenuIndexElements :: Id -> [Index] -> GUI ps ()
closeRadioMenuIndexElements rId indices = do
	idtable <- ioStGetIdTable
	ioId <- accIOEnv ioStGetIOId
	(case getIdParent rId idtable of
		Just parent | checkIdParent parent ioId ->
			I.closeMenuIndexElements I.notRemoveSpecialMenuElements True ioId (idpId parent,Just rId) indices
		_ -> return ())

--	Determine the Ids of all menus.

getMenus :: GUI ps [Id]
getMenus = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return []
	 else let mHs	= menuSystemStateGetMenuHandles mDevice
	      in return (map menuStateHandleGetMenuId (mMenus mHs)))


--	Determine the index position of a menu.

getMenuPos :: Id -> GUI ps (Maybe Index)
getMenuPos id = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return Nothing
	 else let mHs	= menuSystemStateGetMenuHandles mDevice
	      in return (getMenuIndex id 0 (mMenus mHs)))
	where
		getMenuIndex :: Id -> Int -> [MenuStateHandle ps] -> Maybe Int
		getMenuIndex id index (mH:mHs)
			| id==menuStateHandleGetMenuId mH = Just index
			| otherwise = getMenuIndex id (index+1) mHs
		getmenuindex _ _ _ = Nothing


--	Set & Get the title of a menu.

setMenuTitle :: Id -> Title -> GUI ps ()
setMenuTitle id title
	| id==windowMenuId	= return ()
	| otherwise		= I.setMenuTitle id title

getMenuTitle :: Id -> GUI ps (Maybe Title)
getMenuTitle id = accessMenuHandles id menuStateHandleGetTitle
