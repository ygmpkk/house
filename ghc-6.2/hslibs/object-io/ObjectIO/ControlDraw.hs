module ControlDraw where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	Drawing in customised controls
--	********************************************************************************

import StdPicture(toRegion, accClipPicture)
import OSPicture(Draw(..), doDraw, pictSetClipRgn)
import OSRgn(osNewRectRgn, osDisposeRgn, osSectRgn)
import OSSystem(OSWindowMetrics)
import OSWindow(osGrabWindowPictContext, osReleaseWindowPictContext, osScrollbarsAreVisible)
import CommonDef
import WindowHandle
import WindowAccess(getWItemCompoundInfo,  getWItemCustomButtonInfo,  getWItemCustomInfo, getCompoundContentRect)


{-	The following functions apply the current Look function of the given control. -}

{-	drawCompoundLook able parentWindow contextClip itemH
		applies the Look function of the compound control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip (in window coordinates) and the content rectangle of the compound control. 
		The function assumes that itemH refers to a CompoundControl(') which ClipState is valid.
	Note that drawCompoundLook draws in the graphics context of the parent window (os(Grab/Release)WindowPictContext).
		This is done because the ClipState of the CompoundControl is given relative to the left-top of the window.
-}
drawCompoundLook :: OSWindowMetrics -> Bool -> OSWindowPtr -> Rect -> WElementHandle ls ps -> IO (WElementHandle ls ps)
drawCompoundLook wMetrics able wPtr contextClip itemH@(WItemHandle {wItemInfo=wItemInfo}) = do
    contextRgn <- osNewRectRgn contextClip
    clipRgn <- osSectRgn contextRgn (clipRgn itemClip)
    osPict <- osGrabWindowPictContext wPtr
    (_,_,pen,_) <- doDraw (origin-itemPos) (lookPen itemLook) True osPict (pictSetClipRgn clipRgn >> lookFun itemLook selectState updState)
    osReleaseWindowPictContext wPtr osPict
    mapM_ osDisposeRgn [contextRgn,clipRgn]
    let info1 = info{compoundLookInfo=compLookInfo{compoundLook=itemLook{lookPen=pen}}}
    return (itemH{wItemInfo=WCompoundInfo info1})
    where
	itemPos				= wItemPos itemH
	itemSize			= wItemSize itemH
	info				= getWItemCompoundInfo wItemInfo
	(origin,domainRect,hasScrolls)	= (compoundOrigin info,compoundDomain info,(isJust (compoundHScroll info),isJust (compoundVScroll info)))
	visScrolls			= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
	contentRect			= getCompoundContentRect wMetrics visScrolls (posSizeToRect origin itemSize)
	clipRectangle			= rectToRectangle (addVector (toVector (origin-itemPos)) (intersectRects (addVector (toVector (itemPos-origin)) contentRect) contextClip))
	viewFrame			= rectToRectangle contentRect
	updState			= UpdateState{oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}
	compLookInfo			= compoundLookInfo info
	itemLook			= compoundLook compLookInfo
	itemClip			= compoundClip compLookInfo
	selectState			= if able then Able else Unable
	
{-	drawCustomButtonLook able parentWindow contextClip itemH
		applies the Look function of the custom button control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip and the content rectangle of the custom button control. 
		The function assumes that itemH refers to a custom button control.
-}
drawCustomButtonLook :: Bool -> OSWindowPtr -> Rect -> WElementHandle ls ps -> IO (WElementHandle ls ps)
drawCustomButtonLook able wPtr contextClip itemH@(WItemHandle {wItemPtr=wItemPtr,wItemInfo=wItemInfo,wItemPos=wItemPos,wItemSize=wItemSize}) = do
	clipRgn <- osNewRectRgn contextClip
	osPict <- osGrabWindowPictContext wPtr 		-- PA: use window HDC instead of control HDC because of clipstate
	(x,_,pen,_) <- doDraw (zero-wItemPos) (lookPen itemLook) True osPict (pictSetClipRgn clipRgn >> accClipPicture (toRegion clipRectangle) (lookFun itemLook selectState updState))
	osReleaseWindowPictContext wPtr osPict
	osDisposeRgn clipRgn
	return (itemH{wItemInfo=WCustomButtonInfo info{cButtonInfoLook=itemLook{lookPen=pen}}})
	where
		info		= getWItemCustomButtonInfo wItemInfo
		itemLook	= cButtonInfoLook info
		viewFrame	= sizeToRectangle wItemSize
		selectState	= if able then Able else Unable
		clipRectangle	= rectToRectangle (intersectRects (subVector (toVector wItemPos) contextClip) (sizeToRect wItemSize))
		updState	= UpdateState{oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}


{-	drawCustomLook able parentWindow itemH
		applies the Look function of the custom control given the current selectstate (True iff Able).
		Drawing is clipped inside contextClip and the content rectangle of the custom button control. 
		The function assumes that itemH refers to a custom control.
-}
drawCustomLook :: Bool -> OSWindowPtr -> Rect -> WElementHandle ls ps -> IO (WElementHandle ls ps)
drawCustomLook able wPtr contextClip itemH@(WItemHandle {wItemPtr=wItemPtr,wItemInfo=wItemInfo,wItemPos=wItemPos,wItemSize=wItemSize}) = do
	clipRgn <- osNewRectRgn contextClip
	osPict <- osGrabWindowPictContext wPtr		-- PA: use window HDC instead of control HDC because of clipstate	
	(x,_,pen,_) <- doDraw (zero-wItemPos) (lookPen itemLook) True osPict (pictSetClipRgn clipRgn >> accClipPicture (toRegion clipRectangle) (lookFun itemLook selectState updState))
	osReleaseWindowPictContext wPtr osPict
	osDisposeRgn clipRgn
	return (itemH{wItemInfo=WCustomInfo info{customInfoLook=itemLook{lookPen=pen}}})
	where
		info		= getWItemCustomInfo wItemInfo
		itemLook	= customInfoLook info
		viewFrame	= sizeToRectangle wItemSize
		selectState	= if able then Able else Unable
		clipRectangle	= rectToRectangle (intersectRects (subVector (toVector wItemPos) contextClip) (sizeToRect wItemSize))
		updState	= UpdateState{oldFrame=viewFrame,newFrame=viewFrame,updArea=[clipRectangle]}


--	The following functions apply a picture access function to the given control picture.

{-	drawInCompound assumes that the WElementHandle argument refers to a non transparent compound control 
	with a valid ClipState.
-}
drawInCompound :: OSWindowPtr -> Draw x -> Rect -> WElementHandle ls ps -> IO (x,WElementHandle ls ps)
drawInCompound wPtr drawfun contextClip itemH@(WItemHandle {wItemPtr=wItemPtr,wItemInfo=wItemInfo,wItemPos=wItemPos,wItemSize=wItemSize}) = do
	contextRgn <- osNewRectRgn contextClip
	clipRgn <- osSectRgn contextRgn (clipRgn compoundClip)
	osPict <- osGrabWindowPictContext wPtr 		-- PA: use window HDC instead of control HDC because of clipstate	
	(x,_,pen,_) <- doDraw (origin-wItemPos) (lookPen compoundLook) True osPict (pictSetClipRgn clipRgn >> drawfun)
	osReleaseWindowPictContext wPtr osPict
	mapM_ osDisposeRgn [contextRgn,clipRgn]
	return (x, itemH{wItemInfo=WCompoundInfo info{compoundLookInfo=cLookInfo{compoundLook=compoundLook{lookPen=pen}}}})
	where
		info		= getWItemCompoundInfo wItemInfo
		origin		= compoundOrigin info
		cLookInfo 	= compoundLookInfo info
		CompoundLookInfo {compoundLook=compoundLook,compoundClip=compoundClip}	= cLookInfo

drawInCustomButton :: OSWindowPtr -> Draw x -> Rect -> WElementHandle ls ps -> IO (x,WElementHandle ls ps)
drawInCustomButton wPtr drawfun contextClip itemH@(WItemHandle {wItemPtr=wItemPtr,wItemInfo=wItemInfo,wItemPos=wItemPos,wItemSize=wItemSize}) = do
	clipRgn <- osNewRectRgn contextClip 				-- PA+++: clip also inside contextClip
	osPict <- osGrabWindowPictContext wPtr
	(x,_,pen,_) <- doDraw (zero-wItemPos) (lookPen itemLook) True osPict (pictSetClipRgn clipRgn >> accClipPicture (toRegion (sizeToRectangle wItemSize)) drawfun)
	osReleaseWindowPictContext wPtr osPict
	osDisposeRgn clipRgn						-- PA+++: dispose clipping region
	return (x,itemH{wItemInfo=WCustomButtonInfo info{cButtonInfoLook=itemLook{lookPen=pen}}})
	where
		info		= getWItemCustomButtonInfo wItemInfo
		itemLook	= cButtonInfoLook info

drawInCustom :: OSWindowPtr -> Draw x -> Rect -> WElementHandle ls ps -> IO (x,WElementHandle ls ps)
drawInCustom wPtr drawfun contextClip itemH@(WItemHandle {wItemPtr=wItemPtr,wItemInfo=wItemInfo,wItemPos=wItemPos,wItemSize=wItemSize}) = do
	clipRgn <- osNewRectRgn contextClip				-- PA+++: clip also inside contextClip
	osPict <- osGrabWindowPictContext wPtr
	(x,_,pen,_) <- doDraw (zero-wItemPos) (lookPen itemLook) True osPict (pictSetClipRgn clipRgn >> accClipPicture (toRegion (sizeToRectangle wItemSize)) drawfun)
	osReleaseWindowPictContext wPtr osPict
	osDisposeRgn clipRgn						-- PA+++: dispose clipping region
	return (x, itemH{wItemInfo=WCustomInfo info{customInfoLook=itemLook{lookPen=pen}}})
	where
		info		= getWItemCustomInfo wItemInfo
		itemLook	= customInfoLook info