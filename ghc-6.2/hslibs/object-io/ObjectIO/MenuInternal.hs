module MenuInternal( enableMenus, disableMenus, setSelectMenus
				   , closeMenuElements, closeMenuIndexElements, closeMenu
				   , removeSpecialMenuElements, notRemoveSpecialMenuElements
				   , changeMenuSystemState, accessMenuSystemState
				   , setMenuTitle
				   ) where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	********************************************************************************


import	OSMenu
import	MenuEvent(menuHandlesGetMenuStateHandles)
import	DeviceSystemState(menuSystemStateGetMenuHandles)
import	OSTypes(osNoWindowPtr)
import	IOState
import	MenuAccess
import	MenuHandle
import	MenuItems
import	SDISize
import  CommonDef(dumpFatalError, foldrM, removeCheck, remove)
import  MenuCreate(disposeMenuIds, disposeShortcutkeys, disposeSubMenuHandles)
import  Monad(when)
import  Id(removeIdFromIdTable, Id(..))
import  StdIOCommon

type DeltaMenuSystem    ps = OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
type AccessMenuSystem x ps = OSMenuBar -> MenuHandles ps -> IO (x, MenuHandles ps)

menuInternalFatalError :: String -> String -> x
menuInternalFatalError function error
	= dumpFatalError function "menuinternal" error


--	General rules to access MenuHandles:

changeMenuSystemState :: Bool -> DeltaMenuSystem ps -> GUI ps ()
changeMenuSystemState redrawMenus f = do
	(found,mDevice) <- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return ()
	 else do
	        osdinfo <- accIOEnv ioStGetOSDInfo
	        (case getOSDInfoOSMenuBar osdinfo of
	        	Nothing -> menuInternalFatalError "changeMenuSystemState" "could not retrieve OSMenuBar from OSDInfo"
			Just osMenuBar -> do
				newMenus <- liftIO (f osMenuBar (menuSystemStateGetMenuHandles mDevice))
				liftIO (when redrawMenus (drawMenuBar osMenuBar))
				appIOEnv (ioStSetDevice (MenuSystemState newMenus))))
	

accessMenuSystemState :: Bool -> AccessMenuSystem x ps -> GUI ps (Maybe x)
accessMenuSystemState redrawMenus f = do
	(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
	(if not found then return Nothing
	 else do
			osdinfo <- accIOEnv ioStGetOSDInfo
			(case getOSDInfoOSMenuBar osdinfo of
				Nothing -> menuInternalFatalError "accessMenuSystemState" "could not retrieve OSMenuBar from OSDInfo"
				Just osMenuBar -> do
					(x,newMenus) <- liftIO (f osMenuBar (menuSystemStateGetMenuHandles mDevice))
					liftIO (when redrawMenus (drawMenuBar osMenuBar))
					appIOEnv (ioStSetDevice (MenuSystemState newMenus))
					return (Just x)))


{-	Closing a menu.
	Because in a SDI process menus might reside in the process window, the ViewFrame of the
	process window can change size.
	In that case, the layout of the controls should be recalculated, and the window updated.
-}

closeMenu :: Id -> GUI ps ()
closeMenu id = do
	osdInfo <- accIOEnv ioStGetOSDInfo
	(if getOSDInfoDocumentInterface osdInfo==NDI
	 then return ()
	 else
			case getOSDInfoOSMenuBar osdInfo of
				Nothing -> menuInternalFatalError "closeMenu" "could not retrieve OSMenuBar from OSDInfo"
				Just osMenuBar  -> do
						(found,mDevice)	<- accIOEnv (ioStGetDevice MenuDevice)
						(if not found then return ()
						 else
							let
								mHs	= menuSystemStateGetMenuHandles mDevice
								(menus,mHs1) = menuHandlesGetMenuStateHandles mHs
								(found,mH,menus1)	= remove (isMenuWithThisId id) undefined menus
							in 
								if not found then do
									appIOEnv (ioStSetDevice (MenuSystemState mHs{mMenus=menus1}))
								else do
										(sdiSize1,sdiPtr) <- getSDIWindowSize
										keys <- liftIO (filterShortcutkeys osdInfo mH (mKeys mHs))
										rt <- ioStGetReceiverTable
										it <- ioStGetIdTable
										let (_,it1) = removeIdFromIdTable id it
										ioid <- accIOEnv ioStGetIOId
										let (rt2,it2) = closeMenuIds ioid mH rt it1
										ioStSetIdTable it2
										ioStSetReceiverTable rt2
										liftIO (closeSubMenus mH >>
												osMenuRemove (menuStateHandleGetHandle mH) osMenuBar >>
												drawMenuBar osMenuBar)
										let mHs1 = mHs{mMenus=menus1,mKeys=keys}
										appIOEnv (ioStSetDevice (MenuSystemState mHs1))
										(sdiSize2,_) <- getSDIWindowSize
										when (sdiSize1/=sdiSize2) (resizeSDIWindow sdiPtr sdiSize1 sdiSize2)))
	where
		isMenuWithThisId :: Id -> MenuStateHandle ps -> Bool
		isMenuWithThisId id msH = (id == menuStateHandleGetMenuId msH)
			


closeSubMenus :: MenuStateHandle ps -> IO ()
closeSubMenus (MenuStateHandle (MenuLSHandle {mlsHandle=mH})) =
	mapM_ disposeSubMenuHandles (mItems mH)

closeMenuIds :: SystemId -> MenuStateHandle ps -> ReceiverTable -> IdTable -> (ReceiverTable,IdTable)
closeMenuIds pid (MenuStateHandle (MenuLSHandle {mlsHandle=mH})) rt it =
	foldr (disposeMenuIds pid) (rt,it) (mItems mH)

filterShortcutkeys :: OSDInfo -> MenuStateHandle ps -> [Char] -> IO [Char]
filterShortcutkeys osdInfo (MenuStateHandle (MenuLSHandle {mlsHandle=mH})) keys =
	foldrM (disposeShortcutkeys framePtr) keys (mItems mH)
	where
		framePtr	= case (getOSDInfoOSInfo osdInfo) of
			Just info -> osFrame info
			_         -> osNoWindowPtr


--	Enabling and Disabling of Menus:

enableMenus  :: [Id] -> GUI ps ()
enableMenus  ids = changeMenuSystemState True (setSelectMenus ids Able)

disableMenus :: [Id] -> GUI ps ()
disableMenus ids = changeMenuSystemState True (setSelectMenus ids Unable)

setSelectMenus :: [Id] -> SelectState -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
setSelectMenus ids select osMenuBar menus@(MenuHandles {mEnabled=mEnabled,mMenus=mMenus}) = do
	(_,msHs) <- setSelectMenuHandles 0 select osMenuBar mEnabled ids mMenus
	return (menus{mMenus=msHs})
	where	
		setSelectMenuHandles :: Int -> SelectState -> OSMenuBar -> Bool -> [Id] -> [MenuStateHandle ps] -> IO ([Id],[MenuStateHandle ps])
		setSelectMenuHandles zIndex select osMenuBar systemAble ids [] =
			return (ids,[])
		setSelectMenuHandles zIndex select osMenuBar systemAble ids (msH:msHs)
			| null ids = return (ids,msHs)
			| otherwise = do				
				(ids,msH)  <- setSelectMenuHandle   zIndex    select osMenuBar systemAble ids msH
				(ids,msHs) <- setSelectMenuHandles (zIndex+1) select osMenuBar systemAble ids msHs
				return (ids,msH:msHs)
			where
				setSelectMenuHandle :: Int -> SelectState -> OSMenuBar -> Bool -> [Id] -> MenuStateHandle ps -> IO ([Id], MenuStateHandle ps)
				setSelectMenuHandle zIndex select osMenuBar systemAble ids msH@(MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH@(MenuHandle {mMenuId=mMenuId})})) =
					let 
						(containsId,ids1) = removeCheck mMenuId ids
						msH1 = MenuStateHandle mlsH{mlsHandle=mH{mSelect=enabled select}}
					in
						if not containsId then return (ids1,msH)
						else
							if not systemAble then return (ids1,msH1)
							else do								
								(if enabled select then osEnableMenu else osDisableMenu) zIndex osMenuBar
								return (ids1,msH1)

--	Removing menu elements from (sub/radio)menus:

closeMenuElements :: Id -> [Id] -> GUI ps ()
closeMenuElements mId ids = do
	pid <- accIOEnv ioStGetIOId
	rt <- ioStGetReceiverTable
	it <- ioStGetIdTable
	osdInfo <- accIOEnv ioStGetOSDInfo
	result <- accessMenuSystemState True (removeMenusItems osdInfo mId ids pid rt it)
	(case result of
		Nothing			-> return ()
		Just (rt,it)	-> do
			ioStSetIdTable it
			ioStSetReceiverTable rt
			return ())


--	Removing menu elements from (sub/radio)menus by index (counting from 1):

removeSpecialMenuElements	 =	True	-- For closemenuindexelements:        remove elements with special ids
notRemoveSpecialMenuElements =	False	-- For closemenuindexelements: do not remove elements with special ids

closeMenuIndexElements :: Bool -> Bool -> SystemId -> (Id,Maybe Id) -> [Index] -> GUI ps ()
closeMenuIndexElements removeSpecialElements fromRadioMenu pid loc indices = do
	rt <- ioStGetReceiverTable
	it <- ioStGetIdTable
	osdInfo <- accIOEnv ioStGetOSDInfo
	result <- accessMenuSystemState True (removeMenusIndexItems osdInfo removeSpecialElements fromRadioMenu loc indices pid rt it)
	(case result of
		Nothing			-> return ()
		Just (rt,it)	-> do		
			ioStSetIdTable it
			ioStSetReceiverTable rt
			return ())

--	Set & Get the title of a menu.

setMenuTitle :: Id -> Title -> GUI ps ()
setMenuTitle id title = 
	changeMenuSystemState True (setOSMenuTitle id title)
	where
		setOSMenuTitle :: Id -> Title -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
		setOSMenuTitle id title osMenuBar menus@(MenuHandles {mMenus=msHs}) = do
			msHs <- setOSMenusTitle id title osMenuBar msHs
			return (menus{mMenus=msHs})
			where
				setOSMenusTitle :: Id -> Title -> OSMenuBar -> [MenuStateHandle ps] -> IO [MenuStateHandle ps]
				setOSMenusTitle id title osMenuBar (msH:msHs)
					| id == menuStateHandleGetMenuId msH = do					
						osChangeMenuTitle osMenuBar (menuStateHandleGetHandle msH) title
						return (menuStateHandleSetTitle title msH:msHs)
					| otherwise = do
						msHs <- setOSMenusTitle id title osMenuBar msHs
						return (msH:msHs)
				setOSMenusTitle _ _ _ msHs = return msHs
