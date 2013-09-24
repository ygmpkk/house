module MenuItems( addMenusItems, addMenuRadioItems
				, removeMenusItems, removeMenusIndexItems) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	********************************************************************************


import  Id
import  StdGUI(noLS)
import	CommonDef
import  IOState
import  StdMenuElementClass
import  StdMenuDef(MenuRadioItem)
import	MenuCreate
import	MenuHandle
import	ReceiverTable
import	ReceiverHandle
import	OSDocumentInterface
import	OSMenu
import	OSTypes(osNoWindowPtr, OSWindowPtr)


{-	Adding menu elements to (Sub)Menus:
		Items in a (sub)menu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front.
		Open with a position higher than the number of items adds the new elements to the end.
		Open an item on a position adds the item AFTER the item on that position.
	The (Id,Maybe Id) argument indicates where the elements should be added. 
		In case the Maybe Id argument is Nothing, then the elements should be added to the Menu indicated by the Id component. 
		In case the Maybe Id argument is Just id, then the elements should be added to the SubMenu indicated by id.
-}
addMenusItems :: MenuElements m => (Id,Maybe Id) -> Int -> ls -> (m ls ps) -> SystemId -> ReceiverTable -> IdTable -> MenuHandles ps -> OSMenuBar -> GUI ps (ReceiverTable,IdTable,MenuHandles ps)
addMenusItems loc pos ls new pid rt it menus@(MenuHandles {mMenus=mMenus,mKeys=mKeys}) osMenuBar = do
    newItemHs <- menuElementToHandles new
    (let newItemHs1 = map menuElementStateToMenuElementHandle newItemHs
     in case menuIdsAreConsistent pid (fst loc) newItemHs1 rt it of
     		Just (rt,it) -> do
     			(_,rt,it,mHs,keys) <- liftIO (addMenusItems' loc pos ls newItemHs1 pid rt it mMenus mKeys)
			return (rt,it,menus{mMenus=mHs,mKeys=keys})
     		Nothing -> throwGUI ErrorIdsInUse)
    where
	addMenusItems' :: (Id,Maybe Id) -> Int -> ls' -> [MenuElementHandle ls' ps] -> SystemId -> ReceiverTable -> IdTable -> [MenuStateHandle ps] -> [Char] -> IO (ls',ReceiverTable,IdTable,[MenuStateHandle ps],[Char])
	addMenusItems' _ _ ls _ _ rt it [] keys = throwGUI ErrorUnknownObject
	addMenusItems' loc pos ls new pid rt it ((MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH})):msHs) keys = do
	    (opened,ls,rt,it,mH,keys)	<- addMenuItems loc pos ls new pid rt it mH keys
	    let msH = MenuStateHandle mlsH{mlsHandle=mH}
	    (if opened then return (ls,rt,it,msH:msHs,keys)
	     else do
		(ls,rt,it,    msHs, keys) <- addMenusItems' loc pos ls new pid rt it msHs keys
		return (ls,rt,it,msH:msHs,keys))
	    where
		addMenuItems :: (Id,Maybe Id) -> Int -> ls' -> [MenuElementHandle ls' ps] -> SystemId -> ReceiverTable -> IdTable -> MenuHandle ls ps -> [Char] -> IO (Bool,ls',ReceiverTable,IdTable,MenuHandle ls ps,[Char])
		addMenuItems (mId,itemId) pos ls new pid rt it mH@(MenuHandle {mHandle=mHandle,mMenuId=mMenuId,mItems=mItems}) keys
		    | mId /= mMenuId = return (False,ls,rt,it,mH,keys)
		    | isJust itemId = do
			(_,   ls,rt,it,items,keys,_) <- addSubMenuItems' mId (fromJust itemId) pos mHandle ls new pid rt it mItems keys 1
			return (True,ls,rt,it,mH{mItems=items},keys)
		    | otherwise = do
			(_,   ls,rt,it,itemHs,keys,_,_) <- extendMenu' mId mHandle ls new pid rt it mItems keys pos 1
			return (True,ls,rt,it,mH{mItems=itemHs},keys)
		    where
			addSubMenuItems' :: Id -> Id -> Int -> OSMenu -> ls' -> [MenuElementHandle ls' ps] -> SystemId -> ReceiverTable -> IdTable -> [MenuElementHandle ls ps] -> [Char] -> Int -> IO (Bool,ls',ReceiverTable,IdTable,[MenuElementHandle ls ps],[Char],Int)
			addSubMenuItems' menuId itemId pos menu ls new pid rt it (item:items) keys iNr = do
				(opened,ls,rt,it,item,keys,iNr) <- addSubMenuItems menuId itemId pos menu ls new pid rt it item keys iNr
				(if opened
				 then return (opened,ls,rt,it,item:items,keys,iNr)
				 else do
					(opened,ls,rt,it,items,keys,iNr) <- addSubMenuItems' menuId itemId pos menu ls new pid rt it items keys iNr
					return (opened,ls,rt,it,item:items,keys,iNr))
			addSubMenuItems' _ _ _ _ ls _ _ rt it _ keys iNr =
				return (False,ls,rt,it,[],keys,iNr)
			
			addSubMenuItems :: Id -> Id -> Int -> OSMenu -> ls' -> [MenuElementHandle ls' ps] -> SystemId -> ReceiverTable -> IdTable -> MenuElementHandle ls ps -> [Char] -> Int -> IO (Bool,ls',ReceiverTable,IdTable,MenuElementHandle ls ps,[Char],Int)
			addSubMenuItems menuId itemId pos menu ls new pid rt it itemH@(SubMenuHandle {mSubHandle=mSubHandle,mSubMenuId=mSubMenuId,mSubItems=mSubItems}) keys iNr =
				case mSubMenuId of
					Just id | itemId==id -> do
						(_,ls,rt,it,itemHs,keys,_,_) <- extendMenu' menuId mSubHandle ls new pid rt it mSubItems keys pos 1
						return (True,ls,rt,it,itemH{mSubItems=itemHs},keys,iNr+1)
					Nothing -> do
						(opened,ls,rt,it,items,keys,_) <- addSubMenuItems' menuId itemId pos mSubHandle ls new pid rt it mSubItems keys 1
						return (opened,ls,rt,it,itemH{mSubItems=items},keys,iNr+1)
			addSubMenuItems _ _ _ _ ls _ _ rt it itemH@(RadioMenuHandle {mRadioItems=mRadioItems}) keys iNr = do
				return (False,ls,rt,it,itemH,keys,iNr+length mRadioItems)
			addSubMenuItems menuId itemId pos menu ls new pid rt it (MenuListLSHandle itemHs) keys iNr = do
				(opened,ls,rt,it,itemHs,keys,iNr) <- addSubMenuItems' menuId itemId pos menu ls new pid rt it itemHs keys iNr
				return (opened,ls,rt,it,MenuListLSHandle itemHs,keys,iNr)
			addSubMenuItems menuId itemId pos menu ls new pid rt it (MenuExtendLSHandle exLS itemHs) keys iNr = do
				(opened,ls,rt,it,itemHs,keys,iNr) <- addSubMenuItems' menuId itemId pos menu ls new pid rt it itemHs keys iNr
				return (opened,ls,rt,it,MenuExtendLSHandle exLS itemHs,keys,iNr)
			addSubMenuItems menuId itemId pos menu ls new pid rt it (MenuChangeLSHandle chLS itemHs) keys iNr = do
				(opened,ls,rt,it,itemHs,keys,iNr)	<- addSubMenuItems' menuId itemId pos menu ls new pid rt it itemHs keys iNr
				return (opened,ls,rt,it,MenuChangeLSHandle chLS itemHs,keys,iNr)
			addSubMenuItems _ _ _ _ ls _ _ rt it itemH keys iNr = do
				return (False,ls,rt,it,itemH,keys,iNr+1)
			
			extendMenu' :: Id -> OSMenu -> ls' -> [MenuElementHandle ls' ps] -> SystemId -> ReceiverTable -> IdTable -> [MenuElementHandle ls ps] -> [Char] -> Int -> Int -> IO (Bool,ls',ReceiverTable,IdTable,[MenuElementHandle ls ps],[Char],Int,Int)
			extendMenu' menuId menu ls new pid rt it itemHs keys 0 iNr = do
				let newItemHs = MenuChangeLSHandle ls new
				(itemHs,keys) <- extendMenu osMenuBar menu (iNr-1) [newItemHs] itemHs keys
				return (True,undefined,rt,it,itemHs,keys,pos,iNr)
			extendMenu' _ _ ls _ _ rt it [] keys pos iNr =
				return (False,ls,rt,it,[],keys,pos,iNr)
			extendMenu' menuId menu ls new pid rt it (itemH@(MenuItemHandle {}):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs keys (pos-1) (iNr+1)
				return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
			extendMenu' menuId menu ls new pid rt it (itemH@(MenuReceiverHandle _ _):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs keys pos iNr
				return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
			extendMenu' menuId menu ls new pid rt it (itemH@(SubMenuHandle {}):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs keys (pos-1) (iNr+1)
				return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
			extendMenu' menuId menu ls new pid rt it (itemH@(RadioMenuHandle {mRadioItems=mRadioItems}):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs keys (pos-1) (iNr+length mRadioItems)
				return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
			extendMenu' menuId menu ls new pid rt it (itemH@(MenuSeparatorHandle {}):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs keys (pos-1) (iNr+1)
				return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
			extendMenu' menuId menu ls new pid rt it ((MenuListLSHandle itemHs'):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs',keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs' keys pos iNr
				let itemH = MenuListLSHandle itemHs'
				(if opened then return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
				 else do
					(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs  keys pos iNr
					return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr))
			extendMenu' menuId menu ls new pid rt it ((MenuExtendLSHandle exLS itemHs'):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs',keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs' keys pos iNr
				let itemH = MenuExtendLSHandle exLS itemHs'
				(if opened then return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
				 else do
					(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs keys pos iNr
					return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr))
			extendMenu' menuId menu ls new pid rt it ((MenuChangeLSHandle chLS itemHs'):itemHs) keys pos iNr = do
				(opened,ls,rt,it,itemHs',keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs' keys pos iNr
				let itemH = MenuChangeLSHandle chLS itemHs'
				(if opened then return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr)
				 else do
					(opened,ls,rt,it,itemHs,keys,pos,iNr) <- extendMenu' menuId menu ls new pid rt it itemHs  keys pos iNr
					return (opened,ls,rt,it,itemH:itemHs,keys,pos,iNr))	


{-	Adding radio menu items to RadioMenus:
		Items in a RadioMenu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front.
		Open with a position higher than the number of items adds the new elements to the end.
		Open an item on a position adds the item AFTER the item on that position.
	The (Id,Id) argument indicates where the elements should be added. 
		The first Id indicates the Menu, the second Id indicates the RadioMenu.
-}
addMenuRadioItems :: (Id,Id) -> Int -> [MenuRadioItem ps ps] -> OSMenuBar -> MenuHandles ps -> IO (MenuHandles ps)
addMenuRadioItems loc pos new osMenuBar menus@(MenuHandles {mMenus=msHs,mKeys=mKeys}) = do
    (msHs,keys) <- addMenusItems' loc pos new msHs mKeys
    return (menus{mMenus=msHs,mKeys=keys})
    where
	addMenusItems' :: (Id,Id) -> Int -> [MenuRadioItem ps ps] -> [MenuStateHandle ps] -> [Char] -> IO ([MenuStateHandle ps],[Char])
	addMenusItems' _ _ _ [] keys = throwGUI ErrorUnknownObject
	addMenusItems' loc pos new (MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH}):msHs) keys = do
	    (opened,mH,keys) <- addMenuItems loc pos new mH keys
	    let msH = MenuStateHandle mlsH{mlsHandle=mH}
	    (if opened
	     then return (msH:msHs,keys)
	     else do
		(msHs,keys) <- addMenusItems' loc pos new msHs keys
		return (msH:msHs,keys))
	    where
		addMenuItems :: (Id,Id) -> Int -> [MenuRadioItem ps ps] -> (MenuHandle ls ps) -> [Char] -> IO (Bool,MenuHandle ls ps,[Char])
		addMenuItems (mId,itemId) pos new mH@(MenuHandle {mHandle=mHandle,mMenuId=mMenuId,mItems=mItems}) keys
		    | mId /= mMenuId = return (False,mH,keys)
		    | otherwise = do
			   (_,itemHs,keys,_)	<- addSubMenuItems' itemId pos mHandle new mItems keys 1			   
			   return (True,mH{mItems=itemHs},keys)
		    where
			addSubMenuItems' :: Id -> Int -> OSMenu -> [MenuRadioItem ps ps] -> [MenuElementHandle ls ps] -> [Char] -> Int -> IO (Bool,[MenuElementHandle ls ps],[Char],Int)
			addSubMenuItems' itemId pos menu new (itemH:itemHs) keys iNr = do
				(opened,itemH,sIds,iNr) <- addSubMenuItems itemId pos menu new itemH keys iNr
				(if opened then return (opened,itemH:itemHs,keys,iNr)
				 else do
					(opened,itemHs,keys,iNr) <- addSubMenuItems' itemId pos menu new itemHs sIds iNr
					return (opened,itemH:itemHs,keys,iNr))
			addSubMenuItems' _ _ _ _ _ keys iNr =
				throwGUI ErrorUnknownObject
			
			addSubMenuItems :: Id -> Int -> OSMenu -> [MenuRadioItem ps ps] -> MenuElementHandle ls ps -> [Char] -> Int -> IO (Bool,MenuElementHandle ls ps,[Char],Int)
			addSubMenuItems itemId pos menu new itemH@(SubMenuHandle {mSubHandle=handle,mSubItems=itemHs}) keys iNr = do
				(opened,itemHs,keys,_) <- addSubMenuItems' itemId pos handle new itemHs keys 1
				return (opened,itemH{mSubItems=itemHs},keys,iNr+1)
			addSubMenuItems itemId pos menu new itemH@(RadioMenuHandle {mRadioId=mRadioId,mRadioIndex=mRadioIndex,mRadioItems=itemHs}) keys iNr =
				let nrItems = length itemHs
				in if isNothing mRadioId || itemId /= fromJust mRadioId
				   then return (False,itemH,keys,iNr+nrItems)
				   else let newItemHs 	= map (\(a,b,c,f)->radioMenuItemToMenuElementHandle (a,b,c,noLS f)) new
				  	    nrNewItems	= length newItemHs
				  	    pos		= setBetween pos 0 nrItems
				  	    index	= if pos<mRadioIndex then mRadioIndex+nrNewItems else max 1 mRadioIndex
				        in do
				     		(itemHs,keys) <- extendMenu' iNr pos menu newItemHs itemHs keys
						(if nrItems /= 0
						 then return (True,itemH{mRadioIndex=index,mRadioItems=itemHs},keys,iNr+nrItems+nrNewItems)
						 else do
						 	  osMenuItemCheck True menu (mOSMenuItem (itemHs!!(index-1)))
							  return (True,itemH{mRadioIndex=1, mRadioItems=itemHs},keys,iNr+nrItems+nrNewItems))
			addSubMenuItems itemId pos menu new (MenuListLSHandle itemHs) keys iNr = do
				(opened,itemHs,keys,iNr) <- addSubMenuItems' itemId pos menu new itemHs keys iNr
				return (opened,MenuListLSHandle itemHs,keys,iNr)
			addSubMenuItems itemId pos menu new (MenuExtendLSHandle exLS itemHs) keys iNr = do
				(opened,itemHs,keys,iNr) <- addSubMenuItems' itemId pos menu new itemHs keys iNr
				return (opened,MenuExtendLSHandle exLS itemHs,keys,iNr)
			addSubMenuItems itemId pos menu new (MenuChangeLSHandle chLS itemHs) keys iNr = do
				(opened,itemHs,keys,iNr) <- addSubMenuItems' itemId pos menu new itemHs keys iNr
				return (opened,MenuChangeLSHandle chLS itemHs,keys,iNr)
			addSubMenuItems _ _ _ _ itemH@(MenuReceiverHandle {}) keys iNr =
				return (False,itemH,keys,iNr)
			addSubMenuItems _ _ _ _ itemH keys iNr = 
				return (False,itemH,keys,iNr+1)
			
			extendMenu' :: Int -> Int -> OSMenu -> [MenuElementHandle ls ps] -> [MenuElementHandle ls ps] -> [Char] -> IO ([MenuElementHandle ls ps],[Char])
			extendMenu' iNr 0 menu new itemHs keys =
				extendMenu osMenuBar menu (iNr-1) new itemHs keys
			extendMenu' iNr position menu new (itemH:itemHs) keys = do
				(itemHs,keys) <- extendMenu' (iNr+1) (position-1) menu new itemHs keys
				return (itemH:itemHs,keys)
			extendMenu' iNr position menu new items keys =
				extendMenu osMenuBar menu (iNr-1) new items keys	


--	Removing menu elements from (sub/radio)menus:

removeMenusItems :: OSDInfo -> Id -> [Id] -> SystemId -> ReceiverTable -> IdTable -> OSMenuBar -> MenuHandles ps -> IO ((ReceiverTable,IdTable),MenuHandles ps)
removeMenusItems osdInfo mId ids pid rt it _ menus@(MenuHandles {mMenus=mMenus,mKeys=mKeys}) = do
	(rt,it,mHs,keys) <- removeMenusItems' framePtr mId ids pid rt it mMenus mKeys
	return ((rt,it),menus{mMenus=mHs,mKeys=keys})
    where
		framePtr = case getOSDInfoOSInfo osdInfo of
			Just info -> osFrame info
			_         -> osNoWindowPtr
		
		removeMenusItems' :: OSWindowPtr -> Id -> [Id] -> SystemId -> ReceiverTable -> IdTable -> [MenuStateHandle ps] -> [Char] -> IO (ReceiverTable,IdTable,[MenuStateHandle ps],[Char])
		removeMenusItems' _ _ [] _ rt it mHs keys = return (rt,it,mHs,keys)
		removeMenusItems' framePtr mId ids pid rt it (MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH@(MenuHandle {mMenuId=mMenuId})}):mHs) keys
			| mId /= mMenuId = do
				(rt,it,mHs,keys) <- removeMenusItems' framePtr mId ids pid rt it mHs keys
				return (rt,it,MenuStateHandle mlsH:mHs,keys)
			| otherwise = do
				(rt,it,_,mH,keys) <- removeMenuItems framePtr pid rt it ids mH keys
				return (rt,it,MenuStateHandle (mlsH{mlsHandle=mH}):mHs,keys)
			where
				removeMenuItems :: OSWindowPtr -> SystemId -> ReceiverTable -> IdTable -> [Id] -> MenuHandle ls ps -> [Char] -> IO (ReceiverTable,IdTable,[Id],MenuHandle ls ps,[Char])
				removeMenuItems framePtr pid rt it ids mH@(MenuHandle {mHandle=mHandle,mItems=mItems}) keys = do
					(_,rt,it,ids,mItems,keys) <- removeFromMenu' framePtr mHandle pid 1 rt it ids mItems keys
					return (rt,it,ids,mH{mItems=mItems},keys)
					where
						removeFromMenu' :: OSWindowPtr -> OSMenu -> SystemId -> Int -> ReceiverTable -> IdTable -> [Id] -> [MenuElementHandle ls ps] -> [Char] -> IO (Int,ReceiverTable,IdTable,[Id],[MenuElementHandle ls ps],[Char])
						removeFromMenu' framePtr menu pid iNr rt it ids [] keys = return (iNr,rt,it,ids,[],keys)
						removeFromMenu' framePtr menu pid iNr rt it ids (item:items) keys
							| null ids  = return (iNr,rt,it,ids,items,keys)
							| otherwise = do					
									(removed,iNr,rt,it,ids,item, keys)	<- removeFromMenu  framePtr menu pid iNr rt it ids item  keys
									(        iNr,rt,it,ids,items,keys)	<- removeFromMenu' framePtr menu pid iNr rt it ids items keys
									return (iNr,rt,it,ids,if removed then items else (item:items), keys)
						
						removeFromMenu :: OSWindowPtr -> OSMenu -> SystemId -> Int -> ReceiverTable -> IdTable -> [Id] -> MenuElementHandle ls ps -> [Char] -> IO (Bool,Int,ReceiverTable,IdTable,[Id],MenuElementHandle ls ps,[Char])
						removeFromMenu framePtr menu pid iNr rt it ids itemH@(MenuItemHandle {mItemId=mItemId,mItemKey=mItemKey,mOSMenuItem=mOSMenuItem}) keys =
							let (containsItem,ids1) = case mItemId of
									Nothing -> (False,ids)
									Just id -> removeCheck id ids
							in
								if not containsItem then return (containsItem,iNr+1,rt,it,ids1,itemH,keys)
								else do
										(keys,it) <- disposeMenuItemHandle menu iNr itemH (keys, it)
										return (containsItem,iNr,rt,it,ids1,itemH,keys)
						removeFromMenu framePtr menu pid iNr rt it ids itemH@(SubMenuHandle {mSubHandle=mSubHandle,mSubMenuId=mSubMenuId,mSubItems=mSubItems}) keys =
							let (containsItem,ids1) = case mSubMenuId of
									Nothing -> (False,ids)
									Just id -> removeCheck id ids
							in do
									(_,rt,it,ids2,itemHs,keys) <- removeFromMenu' framePtr mSubHandle pid 1 rt it ids1 mSubItems keys
									let itemH1 = itemH{mSubItems=itemHs}
									(if not containsItem then return (containsItem,iNr+1,rt,it,ids2,itemH,keys)
									 else do
											let (rt1,it1) = foldr (disposeMenuIds pid) (rt,it) itemHs
											keys <- foldrM (disposeShortcutkeys framePtr) keys itemHs
											mapM_ disposeSubMenuHandles itemHs
											osSubMenuRemove mSubHandle menu
											return (containsItem,iNr,rt,it,ids2,itemH,keys))
						removeFromMenu framePtr menu pid iNr rt it ids itemH@(RadioMenuHandle {mRadioId=mRadioId,mRadioIndex=mRadioIndex,mRadioItems=mRadioItems}) keys =
							let 
								(containsItem,ids1) = case mRadioId of
									Nothing -> (False,ids)
									Just id -> removeCheck id ids
								items	= confirmRadioMenuIndex mRadioIndex mRadioItems
							in do
									(_,rt,it,ids2,items,keys) <- removeFromMenu' framePtr menu pid iNr rt it ids1 items keys
									(if containsItem then do
										(keys,it) <- foldrM (disposeMenuItemHandle menu iNr) (keys,it) items
										let itemH1 = itemH{mRadioItems=[]}
										return (containsItem,iNr,rt,it,ids2,itemH1,keys)
									 else do
										(index,items) <- checkNewRadioMenuIndex menu iNr items
										let nrNewItems = length items
										let itemH1 = itemH{mRadioItems=items,mRadioIndex=index}
										return (containsItem,iNr+nrNewItems,rt,it,ids2,itemH1,keys))
						removeFromMenu framePtr menu pid iNr rt it ids itemH@(MenuSeparatorHandle {mSepId=mSepId,mOSMenuSeparator=mOSMenuSeparator}) keys =
							let (containsItem,ids1) = case mSepId of
									Nothing -> (False,ids)
									Just id -> removeCheck id ids
							in 
								if not containsItem then return (containsItem,iNr+1,rt,it,ids1,itemH,keys)
								else do
									osMenuRemoveItem mOSMenuSeparator menu
									let (_,it1) = removeIdFromIdTable (fromJust mSepId) it
									return (containsItem,iNr,rt,it1,ids1,itemH,keys)
						removeFromMenu framePtr menu pid iNr rt it ids itemH@(MenuReceiverHandle rH@(ReceiverHandle {rId=rId}) _) keys =
							let (containsItem,ids1) = removeCheck rId ids
							in
								if not containsItem then return (containsItem,iNr,rt,it,ids,itemH,keys)
								else 
									let 
										(_,rt1) = removeReceiverFromReceiverTable rId rt
										(_,it1) = removeIdFromIdTable rId it
									in
										return (containsItem,iNr,rt1,it1,ids,itemH,keys)
						removeFromMenu framePtr menu pid iNr rt it ids (MenuListLSHandle items) keys = do
							(iNr,rt,it,ids,items,keys) <- removeFromMenu' framePtr menu pid iNr rt it ids items keys
							return (False,iNr,rt,it,ids,MenuListLSHandle items,keys)
						removeFromMenu framePtr menu pid iNr rt it ids (MenuExtendLSHandle exLS items) keys = do
							(iNr,rt,it,ids,items,keys) <- removeFromMenu' framePtr menu pid iNr rt it ids items keys
							return (False,iNr,rt,it,ids,MenuExtendLSHandle exLS items,keys)
						removeFromMenu framePtr menu pid iNr rt it ids (MenuChangeLSHandle chLS items) keys = do
							(iNr,rt,it,ids,items,keys) <- removeFromMenu' framePtr menu pid iNr rt it ids items keys
							return (False,iNr,rt,it,ids,MenuChangeLSHandle chLS items,keys)
		removeMenusItems' _ _ _ _ rt it [] keys = return (rt,it,[],keys)


{-	Removing menu elements from (sub/radio)menus by index (counting from 1):
	The second Boolean argument indicates whether the elements to be removed should be removed
		from RadioMenus (True) or (Sub)Menus (False).  
	The (Id,Maybe Id) argument indicates where the elements should be removed. 
		In case the Maybe Id argument is Nothing, then the elements should be removed from 
		the Menu indicated by the Id component. 
		In case the Maybe Id argument is Just id, then the elements should be removed from
		either a SubMenu or a RadioMenu identified by the Id component.
-}


removeMenusIndexItems :: OSDInfo -> Bool -> Bool -> (Id,Maybe Id) -> [Int] -> SystemId -> ReceiverTable -> IdTable -> OSMenuBar -> MenuHandles ps -> IO ((ReceiverTable,IdTable), MenuHandles ps)
removeMenusIndexItems osdInfo alsoSpecials fromRadioMenu loc indices pid rt it _ menus@(MenuHandles {mMenus=mMenus,mKeys=mKeys}) = do
	(rt,it,mHs,keys) <- removeMenusIndexItems' framePtr alsoSpecials fromRadioMenu loc indices pid rt it mMenus mKeys
	return ((rt,it),menus{mMenus=mHs,mKeys=keys})
	where
		framePtr = case getOSDInfoOSInfo osdInfo of
				Just info -> osFrame info
				_         -> osNoWindowPtr

		dec x = x - 1
		
		removeMenusIndexItems' :: OSWindowPtr -> Bool -> Bool -> (Id,Maybe Id) -> [Int] -> SystemId -> ReceiverTable -> IdTable -> [MenuStateHandle ps] -> [Char] -> IO (ReceiverTable,IdTable,[MenuStateHandle ps],[Char])
		removeMenusIndexItems' framePtr alsoSpecials fromRadioMenu loc@(mId,itemId) indices pid rt it ((MenuStateHandle mlsH@(MenuLSHandle {mlsHandle=mH@(MenuHandle {mMenuId=mMenuId})})):mHs) keys
			| mId /= mMenuId = do
				(rt,it,mHs,keys) <- removeMenusIndexItems' framePtr alsoSpecials fromRadioMenu loc indices pid rt it mHs keys
				return (rt,it,(MenuStateHandle mlsH):mHs,keys)
			| otherwise = do
				(_,rt,it,mH,keys) <- removeMenuIndexItems framePtr alsoSpecials fromRadioMenu itemId indices pid rt it mH keys
				return (rt,it,(MenuStateHandle mlsH{mlsHandle=mH}):mHs,keys)
		removeMenusIndexItems' _ _ _ _ _ _ rt it [] keys = return (rt,it,[],keys)
		
		removeMenuIndexItems :: OSWindowPtr -> Bool -> Bool -> Maybe Id -> [Int] -> SystemId -> ReceiverTable -> IdTable -> MenuHandle ls ps -> [Char] -> IO (Bool,ReceiverTable,IdTable,MenuHandle ls ps,[Char])
		removeMenuIndexItems framePtr alsoSpecials fromRadioMenu itemId indices pid rt it mH@(MenuHandle {mHandle=mHandle,mItems=mItems}) keys =
			case itemId of
				Nothing -> do
						(_,_,rt,it,mItems,keys)	<- removeItems' framePtr alsoSpecials mHandle pid 1 indices rt it mItems keys
						return (True,rt,it,mH{mItems=mItems},keys)
				Just id -> do
						(done,_,rt,it,mItems,keys) <- removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid id indices mHandle 1 rt it mItems keys
						return (done,rt,it,mH{mItems=mItems},keys)
			where
				removeIndexsFromMenu' :: OSWindowPtr -> Bool -> Bool -> SystemId -> Id -> [Int] -> OSMenu -> Int -> ReceiverTable -> IdTable -> [MenuElementHandle ls ps] -> [Char] -> IO (Bool,Int,ReceiverTable,IdTable,[MenuElementHandle ls ps],[Char])
				removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it [] keys =
					return (False,iNr,rt,it,[],keys)
				removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it (item:items) keys = do
					(done,iNr,rt,it,item,keys) <- removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it item keys
					(if done then return (done,iNr,rt,it,item:items,keys)
					 else do
						(done,iNr,rt,it,items,keys) <- removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it items keys
						return (done,iNr,rt,it,item:items,keys))
				
				removeIndexsFromMenu :: OSWindowPtr -> Bool -> Bool -> SystemId -> Id -> [Int] -> OSMenu -> Int -> ReceiverTable -> IdTable -> MenuElementHandle ls ps -> [Char] -> IO (Bool,Int,ReceiverTable,IdTable,MenuElementHandle ls ps,[Char])
				removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it subH@(SubMenuHandle {mSubHandle=mSubHandle,mSubMenuId=mSubMenuId,mSubItems=mSubItems}) keys
					| isJust mSubMenuId && itemId==fromJust mSubMenuId && not fromRadioMenu = do
						(_,_,rt,it,items,keys) <- removeItems' framePtr alsoSpecials mSubHandle pid 1 indices rt it mSubItems keys
						return (True,iNr+1,rt,it,subH{mSubItems=items},keys)
					| otherwise = do
						(done,_,rt,it,items,keys) <- removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid itemId indices mSubHandle 1 rt it mSubItems keys
						return (done,iNr+1,rt,it,subH{mSubItems=items},keys)
				removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it radioH@(RadioMenuHandle {mRadioId=mRadioId,mRadioIndex=mRadioIndex,mRadioItems=mRadioItems}) keys
					| isNothing mRadioId || itemId/=fromJust mRadioId || not fromRadioMenu =
						return (False,iNr,rt,it,radioH,keys)
					| otherwise =
						let
							iNrIndices = map (\index->index+iNr-1) indices
							items	   = confirmRadioMenuIndex mRadioIndex mRadioItems
						in do
							(_,_,rt,it,items,keys) <- removeItems' framePtr alsoSpecials menu pid iNr iNrIndices rt it items keys
							(index,items) <- checkNewRadioMenuIndex menu iNr items
							return (True,iNr,rt,it,radioH{mRadioItems=items,mRadioIndex=index},keys)
				removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it (MenuListLSHandle mListItems) keys = do
					(removed,iNr,rt,it,mListItems,keys)	<- removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it mListItems keys
					return (removed,iNr,rt,it,MenuListLSHandle mListItems,keys)
				removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it (MenuExtendLSHandle exLS mExtendItems) keys = do
					(removed,iNr,rt,it,mExtendItems,keys) <- removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it mExtendItems keys
					return (removed,iNr,rt,it,MenuExtendLSHandle exLS mExtendItems,keys)
				removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it (MenuChangeLSHandle chLS mChangeItems) keys = do
					(removed,iNr,rt,it,mChangeItems,keys) <- removeIndexsFromMenu' framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it mChangeItems keys
					return (removed,iNr,rt,it,MenuChangeLSHandle chLS mChangeItems,keys)
				removeIndexsFromMenu _ _ _ _ _ _ _ iNr rt it h sIds =
					return (False,iNr+1,rt,it,h,sIds)
				
				removeItems' :: OSWindowPtr -> Bool -> OSMenu -> SystemId -> Int -> [Int] -> ReceiverTable -> IdTable -> [MenuElementHandle ls ps] -> [Char] -> IO (Int,[Int],ReceiverTable,IdTable,[MenuElementHandle ls ps],[Char])
				removeItems' framePtr alsoSpecials menu pid iNr indices rt it [] keys = 
					return (iNr,indices,rt,it,[],keys)
				removeItems' framePtr alsoSpecials menu pid iNr indices rt it (item:items) keys
					| null indices = return (iNr,indices,rt,it,items,keys)
					| otherwise = do
							(removed,iNr,indices,rt,it,item, keys) <- removeItems  framePtr alsoSpecials menu pid iNr indices rt it item  keys
							(        iNr,indices,rt,it,items,keys) <- removeItems' framePtr alsoSpecials menu pid iNr indices rt it items keys
							return (iNr,indices,rt,it, if removed then items else item:items, keys)			
				
				removeItems :: OSWindowPtr -> Bool -> OSMenu -> SystemId -> Int -> [Int] -> ReceiverTable -> IdTable -> MenuElementHandle ls ps -> [Char] -> IO (Bool,Int,[Int],ReceiverTable,IdTable,MenuElementHandle ls ps,[Char])
				removeItems framePtr alsoSpecials menu pid iNr indices rt it subH@(SubMenuHandle {mSubHandle=mSubHandle,mSubMenuId=mSubMenuId,mSubItems=mSubItems}) keys =
					let (containsItem,indices1) = removeCheck iNr indices
					in
						if not containsItem || (specialId && not alsoSpecials)
						then return (False,iNr+1,indices1,rt,it,subH,keys)
						else do
								let (rt1,it1)	= foldr (disposeMenuIds pid) (rt,it) mSubItems
								keys1 <- foldrM (disposeShortcutkeys framePtr) keys mSubItems
								mapM_ disposeSubMenuHandles mSubItems
								osSubMenuRemove mSubHandle menu
								let indices2 = map dec indices1
								return (containsItem,iNr,indices2,rt1,it1,subH,keys1)
					where
						specialId = isJust mSubMenuId && isSpecialId (fromJust mSubMenuId)
				removeItems framePtr alsoSpecials menu pid iNr indices rt it itemH@(MenuItemHandle {mItemId=mItemId,mItemKey=mItemKey,mOSMenuItem=mOSMenuItem}) keys =
					let (containsItem,indices1) = removeCheck iNr indices
					in
						if not containsItem || (specialId && not alsoSpecials)
						then return (False,iNr+1,indices1,rt,it,itemH,keys)
						else do
								(keys1,it1) <- disposeMenuItemHandle menu iNr itemH (keys,it)
								let indices2 = map dec indices1
								return (containsItem,iNr,indices2,rt,it1,itemH,keys1)
						where
							specialId = isJust mItemId && isSpecialId (fromJust mItemId)
				removeItems framePtr alsoSpecials menu pid iNr indices rt it radioH@(RadioMenuHandle {mRadioId=mRadioId,mRadioItems=mRadioItems}) keys =
					let (containsItem,indices1) = removeCheck iNr indices
					in
						if not containsItem || (specialId && not alsoSpecials)
						then let 
									nrItems	= length mRadioItems									
									indices2 = map ((+) (nrItems-1)) indices1
							 in 
									return (False,iNr+nrItems,indices2,rt,it,radioH,keys)
						else do
								(keys1,it1) <- foldrM (disposeMenuItemHandle menu iNr) (keys,it) mRadioItems
								let radioH1	  = radioH{mRadioItems=[]}
								let indices2  = map dec indices1
								return (containsItem,iNr,indices2,rt,it1,radioH1,keys1)
					where
						specialId = isJust mRadioId && isSpecialId (fromJust mRadioId)
				removeItems framePtr alsoSpecials menu pid iNr indices rt it sepH@(MenuSeparatorHandle {mSepId=mSepId,mOSMenuSeparator=mOSMenuSeparator}) keys =
					let (containsItem,indices1) = removeCheck iNr indices
					in
						if not containsItem || (specialId && not alsoSpecials)
						then return (False,iNr+1,indices1,rt,it,sepH,keys)
						else do
								osMenuRemoveItem mOSMenuSeparator menu
								let (_,it1) = removeIdFromIdTable (fromJust mSepId) it
								let indices2 = map dec indices1
								return (containsItem,iNr,indices2,rt,it1,sepH,keys)
					where
						specialId = isJust mSepId && isSpecialId (fromJust mSepId)
				removeItems framePtr alsoSpecials menu pid iNr indices rt it recH@(MenuReceiverHandle (ReceiverHandle{rId=rId}) _) keys =
					let (containsItem,indices1) = removeCheck iNr indices
					in
						if not containsItem
						then return (False,iNr,indices,rt,it,recH,keys)
						else 
								let 
									(_,rt) = removeReceiverFromReceiverTable rId rt
									(_,it) = removeIdFromIdTable rId it
								in
									return (containsItem,iNr,indices,rt,it,recH,keys)
				removeItems framePtr alsoSpecials menu pid iNr indices rt it (MenuListLSHandle mListItems) keys = do
					(iNr,indices,rt,it,mListItems,keys) <- removeItems' framePtr alsoSpecials menu pid iNr indices rt it mListItems keys
					return (False,iNr,indices,rt,it,MenuListLSHandle mListItems,keys)
				removeItems framePtr alsoSpecials menu pid iNr indices rt it (MenuExtendLSHandle exLS mExtendItems) keys = do
					(iNr,indices,rt,it,items,keys)	<- removeItems' framePtr alsoSpecials menu pid iNr indices rt it mExtendItems keys
					return (False,iNr,indices,rt,it,MenuExtendLSHandle exLS items,keys)
				removeItems framePtr alsoSpecials menu pid iNr indices rt it (MenuChangeLSHandle chLS mChangeItems) keys = do
					(iNr,indices,rt,it,items,keys)	<- removeItems' framePtr alsoSpecials menu pid iNr indices rt it mChangeItems keys
					return (False,iNr,indices,rt,it,MenuChangeLSHandle chLS items,keys)


{-	confirmRadioMenuIndex ensures that only the menu item at the index position in the list has a True mItemMark field (counted from 1). -}

confirmRadioMenuIndex :: Int -> [MenuElementHandle ls ps] -> [MenuElementHandle ls ps]
confirmRadioMenuIndex i (itemH@(MenuItemHandle {}):itemHs) =
	(itemH{mItemMark=i==1}:confirmRadioMenuIndex (i-1) itemHs)
confirmRadioMenuIndex _ [] = []

{-	checkNewRadioMenuIndex yields the Index of the first checked menu item in the list.
	If the list is empty, then 0 is returned and no menu item is checked.
	If there is no checked menu item then 1 is returned, and the first menu item (if
	present) in the list is checked.
-}
checkNewRadioMenuIndex :: OSMenu -> Int -> [MenuElementHandle ls ps] -> IO (Int,[MenuElementHandle ls ps])
checkNewRadioMenuIndex menu iNr [] 	= return (0,[])
checkNewRadioMenuIndex menu iNr itemHs@(_:_) =
    let (found,index) = getNewRadioMenuIndex 1 itemHs
    in if found then return (index,itemHs)
       else do
		itemHs <- checkFirstRadioItem menu iNr itemHs
		return (1,itemHs)
    where
	getNewRadioMenuIndex :: Int -> [MenuElementHandle ls ps] -> (Bool,Int)
	getNewRadioMenuIndex index (itemH@(MenuItemHandle {mItemMark=mItemMark}):itemHs)
		| mItemMark   = (mItemMark,index)
		| otherwise   = getNewRadioMenuIndex (index+1) itemHs				
	getNewRadioMenuIndex index [] = (False,index)
	
	checkFirstRadioItem :: OSMenu -> Int -> [MenuElementHandle ls ps] -> IO [MenuElementHandle ls ps]
	checkFirstRadioItem menu iNr (itemH@(MenuItemHandle {mOSMenuItem=mOSMenuItem}):itemHs) = do
		osMenuItemCheck True menu mOSMenuItem
		return (itemH{mItemMark=True}:itemHs)
	checkFirstRadioItem _ _ [] = return []
