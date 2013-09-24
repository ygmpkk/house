module StdControlClass (Controls(..), WindowHandle.ControlState) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--
--	StdControlClass define the standard set of controls instances.
--	********************************************************************************


import CommonDef
import IOState
import OSFont
import OSTypes
import OSWindow
import StdControlAttribute
import WindowHandle
import WindowValidate(validateViewDomain)
import ControlValidate
import StdIOCommon
import List(find)
import StdPicture(stdUnfillUpdAreaLook)
import OSPicture(setPenAttribute, defaultPen)
import OSRgn(osNoRgn)



class Controls cdef where
	controlToHandles :: cdef ls ps -> GUI ps [ControlState ls ps]

instance (Controls c) => Controls (AddLS c) where
	controlToHandles (AddLS addLS addDef)
		= do {
			cs <- controlToHandles addDef;
			return [wElementHandleToControlState
				(WExtendLSHandle addLS (map controlStateToWElementHandle cs))
			       ]
		     }

instance (Controls c) => Controls (NewLS c) where
	controlToHandles (NewLS newLS newDef)
		= do {
			cs <- controlToHandles newDef;
			return [wElementHandleToControlState
				(WChangeLSHandle newLS (map controlStateToWElementHandle cs))
			       ]
		      }

instance (Controls c) => Controls (ListLS c) where
	controlToHandles (ListLS cDefs)
		= do {
			css <- sequence (map controlToHandles cDefs);
			return [wElementHandleToControlState (WListLSHandle (map controlStateToWElementHandle (concat css)))]
		     }

instance Controls NilLS where
	controlToHandles NilLS
		= return []


instance (Controls c1,Controls c2) => Controls (TupLS c1 c2) where
	controlToHandles (c1 :+: c2)
		= do {
			cs1 <- controlToHandles c1;
			cs2 <- controlToHandles c2;
			return (cs1 ++ cs2)
		     }

instance Controls ButtonControl where
	controlToHandles (ButtonControl textLine atts)
		= do {
			wMetrics <- accIOEnv ioStGetOSWindowMetrics;
			size     <- liftIO (getButtonSize wMetrics textLine cWidth);
			return
				[wElementHandleToControlState
					(WItemHandle
						{ wItemId         = getIdAttribute atts
						, wItemNr         = 0
						, wItemKind       = IsButtonControl
						, wItemShow	  = not (any isControlHide atts)
						, wItemSelect	  = getSelectStateAttribute atts
						, wItemInfo       = WButtonInfo (ButtonInfo {buttonInfoText=textLine})
						, wItemAtts       = filter (not . redundantAttribute) atts
						, wItems	  = []
						, wItemVirtual	  = False
						, wItemPos        = zero
						, wItemSize       = size
						, wItemPtr        = osNoWindowPtr
						, wItemLayoutInfo = undefined
						})
				]
		  }
		where
			cWidth = getControlWidthAttribute atts

			getButtonSize :: OSWindowMetrics -> String -> Maybe ControlWidth -> IO Size
			getButtonSize wMetrics _ (Just (PixelWidth reqW))
				= return (Size {w=wOK,h=hOK})
				where
					wOK = max (osGetButtonControlMinWidth wMetrics) reqW
					hOK = osGetButtonControlHeight wMetrics
			getButtonSize wMetrics _ (Just (TextWidth wtext))
				= do {
					width <- getDialogFontTextWidth wMetrics wtext;
					let wOK = max (osGetButtonControlMinWidth wMetrics) width
					in  return (Size {w=wOK,h=hOK})
				  }
				where
					hOK = osGetButtonControlHeight wMetrics
			getButtonSize wMetrics _ (Just (ContentWidth wtext))
				= do {
					(width,hOK) <- osGetButtonControlSize wMetrics wtext;
					let wOK = max (osGetButtonControlMinWidth wMetrics) width
					in  return (Size {w=wOK,h=hOK})
				  }
			getButtonSize wMetrics text Nothing
				= do {
					(width,hOK) <- osGetButtonControlSize wMetrics text;
					let wOK = max (osGetButtonControlMinWidth wMetrics) width
					in  return (Size {w=wOK,h=hOK})
				  }



instance Controls CheckControl where
	controlToHandles (CheckControl items layout atts)
		= do {
			wMetrics <- accIOEnv ioStGetOSWindowMetrics;
			infoItems <- liftIO (mapM (checkItemToInfo wMetrics) items);
			return	[wElementHandleToControlState
					(WItemHandle
						{	wItemId		= getIdAttribute atts
						,	wItemNr		= 0
						,	wItemKind	= IsCheckControl
						,	wItemShow	= not (any isControlHide atts)
						,	wItemSelect	= getSelectStateAttribute atts
						,	wItemInfo	= WCheckInfo (CheckInfo
										{	checkItems = infoItems
										,	checkLayout= validateLayout (length items) layout
										})
						,	wItemAtts	= filter (not . redundantAttribute) atts
						,	wItems		= []
						,	wItemVirtual	= False
						,	wItemPos	= zero
						,	wItemSize	= zero
						,	wItemPtr	= osNoWindowPtr
						,	wItemLayoutInfo	= undefined
						})
				]
		  }
		where
			checkItemToInfo :: OSWindowMetrics -> CheckControlItem ps (ls,ps) -> IO (CheckItemInfo ls ps)
			checkItemToInfo wMetrics (text,Just (PixelWidth reqW),mark,f) =
				let
					wOK				= max (osGetCheckControlItemMinWidth wMetrics) reqW
					hOK				= osGetCheckControlItemHeight wMetrics
				in
					return (CheckItemInfo {checkItem=(text,wOK,mark,f)
								,checkItemSize=Size{w=wOK,h=hOK}
								,checkItemPos=zero
								,checkItemPtr=osNoWindowPtr})

			checkItemToInfo wMetrics (text,Just (TextWidth wtext),mark,f) = do
				w <- getDialogFontTextWidth wMetrics wtext
				let wOK	= max (osGetCheckControlItemMinWidth wMetrics) w
				    hOK	= osGetCheckControlItemHeight wMetrics
				return (CheckItemInfo{checkItem=(text,wOK,mark,f)
							,checkItemSize=Size{w=wOK,h=hOK}
							,checkItemPos=zero
							,checkItemPtr=osNoWindowPtr})
			checkItemToInfo wMetrics (text,Just (ContentWidth wtext),mark,f) = do
				(w,hOK) <- osGetCheckControlItemSize wMetrics wtext
				let wOK = max (osGetCheckControlItemMinWidth wMetrics) w
				return (CheckItemInfo{checkItem=(text,wOK,mark,f)
							,checkItemSize=Size{w=wOK,h=hOK}
							,checkItemPos=zero
							,checkItemPtr=osNoWindowPtr})
			checkItemToInfo wMetrics (text,Nothing,mark,f) = do
				(w,hOK) <- osGetCheckControlItemSize wMetrics text
				let wOK	= max (osGetCheckControlItemMinWidth wMetrics) w
				return (CheckItemInfo{checkItem=(text,wOK,mark,f)
							,checkItemSize=Size{w=wOK,h=hOK}
							,checkItemPos=zero
							,checkItemPtr=osNoWindowPtr})

instance Controls c => Controls (CompoundControl c) where
	controlToHandles (CompoundControl controls atts)
		= do {
			cs <-  controlToHandles controls;
			return [wElementHandleToControlState
				(WItemHandle
				{ wItemId	= getIdAttribute atts
				, wItemNr	= 0
				, wItemKind	= IsCompoundControl
				, wItemShow	= isNothing (find isControlHide atts)
				, wItemSelect	= getSelectStateAttribute atts
				, wItemInfo	= WCompoundInfo (CompoundInfo
							{ compoundDomain   = rectangleToRect domain
							, compoundOrigin   = origin
							, compoundHScroll  = if hasHScroll then (Just hScrollInfo) else Nothing
							, compoundVScroll  = if hasVScroll then (Just vScrollInfo) else Nothing
							, compoundLookInfo = CompoundLookInfo
								{ compoundLook= LookInfo
									{ lookFun = lookFun
									, lookPen = pen
									, lookSysUpdate = sysLook
									}
								, compoundClip=ClipState{clipRgn=osNoRgn,clipOk=False}
								}
							})
				, wItemAtts	= filter (not . redundantAttribute) atts
				, wItems	= map controlStateToWElementHandle cs
				, wItemVirtual	= False
				, wItemPos	= zero
				, wItemSize	= zero
				, wItemPtr	= osNoWindowPtr
				, wItemLayoutInfo	= undefined
				})
			]
		}
		where
			(hasHScroll,hScrollAtt)		= cselect isControlHScroll undefined atts
			(hasVScroll,vScrollAtt)		= cselect isControlVScroll undefined atts
			(_,lookAtt)			= cselect isControlLook (ControlLook True stdUnfillUpdAreaLook) atts
			(sysLook,lookFun)		= getControlLookAtt lookAtt
			defaultDomain			= ControlViewDomain viewDomainRange{corner1=zero}
			(_,domainAtt)			= cselect isControlViewDomain defaultDomain atts
			domain				= validateViewDomain (getControlViewDomainAtt domainAtt)
			(_,originAtt)			= cselect isControlOrigin (ControlOrigin (corner1 domain)) atts
			origin				= validateOrigin domain (getControlOriginAtt originAtt)
			hScrollInfo			= ScrollInfo
								{ scrollFunction = getControlHScrollFun hScrollAtt
								, scrollItemPos	 = zero
								, scrollItemSize = zero
								, scrollItemPtr	 = osNoWindowPtr
								}
			vScrollInfo			= ScrollInfo
								{ scrollFunction = getControlVScrollFun vScrollAtt
								, scrollItemPos	 = zero
								, scrollItemSize = zero
								, scrollItemPtr	 = osNoWindowPtr
								}
			pen				= getInitialPen atts


instance Controls CustomButtonControl where
	controlToHandles (CustomButtonControl (Size {w=w,h=h}) controlLook atts) = do
		let size = Size{w=max 0 w,h=max 0 h}
		return [wElementHandleToControlState
			(WItemHandle
			{ wItemId		= getIdAttribute atts
			, wItemNr		= 0
			, wItemKind		= IsCustomButtonControl
			, wItemShow		= not (any isControlHide atts)
			, wItemSelect		= getSelectStateAttribute atts
			, wItemInfo		= WCustomButtonInfo (CustomButtonInfo {cButtonInfoLook=LookInfo{lookFun=controlLook,lookPen=getInitialPen atts,lookSysUpdate=True}})
			, wItemAtts		= filter (not . redundantAttribute) atts
			, wItems		= []
			, wItemVirtual		= False
			, wItemPos		= zero
			, wItemSize		= size
			, wItemPtr		= osNoWindowPtr
			, wItemLayoutInfo	= undefined
			})
		       ]

instance Controls CustomControl where
	controlToHandles (CustomControl (Size {w=w,h=h}) controlLook atts) = do
		let size = Size {w=max 0 w,h=max 0 h}
		return [wElementHandleToControlState
			(WItemHandle
			{ wItemId		= getIdAttribute atts
			, wItemNr		= 0
			, wItemKind		= IsCustomControl
			, wItemShow		= not (any isControlHide atts)
			, wItemSelect		= getSelectStateAttribute atts
			, wItemInfo		= WCustomInfo (CustomInfo {customInfoLook=LookInfo{lookFun=controlLook,lookPen=getInitialPen atts,lookSysUpdate=True}})
			, wItemAtts		= filter (not . redundantAttribute) atts
			, wItems		= []
			, wItemVirtual		= False
			, wItemPos		= zero
			, wItemSize		= size
			, wItemPtr		= osNoWindowPtr
			, wItemLayoutInfo	= undefined
			})
		       ]

instance Controls EditControl where
	controlToHandles (EditControl textLine cWidth nrLines atts)
		= do {
			wMetrics <- accIOEnv ioStGetOSWindowMetrics;
			size     <- liftIO (getEditSize wMetrics nrLines cWidth);
			return
				[wElementHandleToControlState
					(WItemHandle
						{ wItemId         = getIdAttribute atts
						, wItemNr         = 0
						, wItemKind       = IsEditControl
						, wItemShow	  = not (any isControlHide atts)
						, wItemSelect	  = getSelectStateAttribute atts
						, wItemInfo       = WEditInfo (EditInfo
							                  { editInfoText    = textLine
							                  , editInfoWidth   = w size
							                  , editInfoNrLines = nrLines
							                  })
						, wItemAtts       = filter (not . redundantAttribute) atts
						, wItems	  = []
						, wItemVirtual	  = False
						, wItemPos        = zero
						, wItemSize       = size
						, wItemPtr        = osNoWindowPtr
						, wItemLayoutInfo = undefined
						})
				]
		  }
		where
			getEditSize :: OSWindowMetrics -> Int -> ControlWidth -> IO Size
			getEditSize wMetrics nrLines (PixelWidth reqW)
				= do {
					(width,hOK) <- osGetEditControlSize wMetrics reqW nrLines;
					let wOK = max (osGetEditControlMinWidth wMetrics) width
					in  return (Size {w=wOK,h=hOK})
				  }
			getEditSize wMetrics nrLines (TextWidth wtext)
				= do {
					width        <- getDialogFontTextWidth wMetrics wtext;
					(width1,hOK) <- osGetEditControlSize wMetrics width nrLines;
					let wOK = max (osGetEditControlMinWidth wMetrics) width1
					in  return (Size {w=wOK,h=hOK})
				  }
			getEditSize wMetrics nrLines (ContentWidth wtext)
				= do {
					width        <- getDialogFontTextWidth wMetrics (wtext++"mm");
					(width1,hOK) <- osGetEditControlSize wMetrics width nrLines;
					let wOK = max (osGetEditControlMinWidth wMetrics) width1
					in  return (Size {w=wOK,h=hOK})
				  }

instance (Controls c) => Controls (LayoutControl c) where
	controlToHandles (LayoutControl controls atts)
		= do {
			cs <- controlToHandles controls;
			return [wElementHandleToControlState
				(WItemHandle
				{	wItemId		= getIdAttribute atts
				,	wItemNr		= 0
				,	wItemKind	= IsLayoutControl
				,	wItemShow	= not (any isControlHide atts)
				,	wItemSelect	= getSelectStateAttribute atts
				,	wItemInfo	= NoWItemInfo
				,	wItemAtts	= filter (not . redundantAttribute) atts
				,	wItems		= map controlStateToWElementHandle cs
				,	wItemVirtual	= False
				,	wItemPos	= zero
				,	wItemSize	= zero
				,	wItemPtr	= osNoWindowPtr
				,	wItemLayoutInfo	= undefined
				})
			]
		}

instance Controls PopUpControl where
	controlToHandles (PopUpControl popUpItems index atts)
		= do {
			wMetrics <- accIOEnv ioStGetOSWindowMetrics;
			size <- liftIO (getPopUpSize wMetrics (map fst popUpItems) cWidth);
			return [wElementHandleToControlState
				(WItemHandle
				{ wItemId		= getIdAttribute atts
				, wItemNr		= 0
				, wItemKind		= IsPopUpControl
				, wItemShow		= not (any isControlHide atts)
				, wItemSelect		= getSelectStateAttribute atts
				, wItemInfo		= WPopUpInfo (PopUpInfo
								{	popUpInfoItems = popUpItems
								,	popUpInfoIndex = validatePopUpIndex (length popUpItems) index
								,	popUpInfoEdit  = Nothing
								})
				, wItemAtts		= filter (not . redundantAttribute) atts
				, wItems		= []
				, wItemVirtual		= False
				, wItemPos		= zero
				, wItemSize		= size
				, wItemPtr		= osNoWindowPtr
				, wItemLayoutInfo	= undefined
				})]
		}
		where
			cWidth						= getControlWidthAttribute atts

			getPopUpSize :: OSWindowMetrics -> [String] -> (Maybe ControlWidth) -> IO Size
			getPopUpSize wMetrics _ (Just (PixelWidth reqW)) =
				let wOK	= max (osGetPopUpControlMinWidth wMetrics) reqW
				    hOK = osGetPopUpControlHeight wMetrics
				in
				    return (Size{w=wOK,h=hOK})
			getPopUpSize wMetrics _ (Just (TextWidth wtext)) = do
				w <- getDialogFontTextWidth wMetrics wtext
				(let wOK = max (osGetPopUpControlMinWidth wMetrics) w
				     hOK = osGetPopUpControlHeight wMetrics
				 in return (Size{w=wOK,h=hOK}))
			getPopUpSize wMetrics _ (Just (ContentWidth wtext)) = do
				(w,hOK) <- osGetPopUpControlSize wMetrics [wtext]
				(let wOK = max (osGetPopUpControlMinWidth wMetrics) w
				 in return (Size{w=wOK,h=hOK}))
			getPopUpSize wMetrics itemtexts Nothing = do
				(w,hOK) <- osGetPopUpControlSize wMetrics (if null itemtexts then ["MMMMMMMMMM"] else itemtexts)
				(let wOK = max (osGetPopUpControlMinWidth wMetrics) w
				 in return (Size{w=wOK,h=hOK}))


instance Controls RadioControl where
	controlToHandles (RadioControl items layout index atts)
		= do {
			wMetrics <- accIOEnv ioStGetOSWindowMetrics;
			infoItems <- liftIO (mapM (radioItemToInfo wMetrics) items);
			return [wElementHandleToControlState
				(WItemHandle
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsRadioControl
				,	wItemShow		= not (any isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= let nrItems = length items in
									WRadioInfo (RadioInfo
										{	radioItems = infoItems
										,	radioLayout= validateLayout nrItems layout
										,	radioIndex = setBetween index 1 nrItems
										})
				,	wItemAtts		= filter (not . redundantAttribute) atts
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= osNoWindowPtr
				,	wItemLayoutInfo	= undefined
				})]
		}
		where
			radioItemToInfo :: OSWindowMetrics -> (RadioControlItem ps (ls,ps)) -> IO (RadioItemInfo ls ps)

			radioItemToInfo wMetrics (text,Just (PixelWidth reqW),f) =
				let
					wOK = max (osGetRadioControlItemMinWidth wMetrics) reqW
					hOK = osGetRadioControlItemHeight wMetrics
				in
					return (RadioItemInfo {radioItem=(text,wOK,f)
							, radioItemSize=Size{w=wOK,h=hOK}
							, radioItemPos=zero
							, radioItemPtr=osNoWindowPtr})
			radioItemToInfo wMetrics (text,Just (TextWidth wtext),f) = do
				w <- getDialogFontTextWidth wMetrics wtext
			  	let wOK = max (osGetRadioControlItemMinWidth wMetrics) w
				    hOK = osGetRadioControlItemHeight wMetrics
				return (RadioItemInfo{radioItem=(text,wOK,f)
						,radioItemSize=Size{w=wOK,h=hOK}
						,radioItemPos=zero
						,radioItemPtr=osNoWindowPtr})
			radioItemToInfo wMetrics (text,Just (ContentWidth wtext),f) = do
				(w,hOK) <- osGetRadioControlItemSize wMetrics wtext
			  	let wOK	= max (osGetRadioControlItemMinWidth wMetrics) w
				return (RadioItemInfo{radioItem=(text,wOK,f)
						,radioItemSize=Size{w=wOK,h=hOK}
						,radioItemPos=zero
						,radioItemPtr=osNoWindowPtr})
			radioItemToInfo wMetrics (text,Nothing,f) = do
				(w,hOK)	<- osGetRadioControlItemSize wMetrics text
			  	let wOK	= max (osGetRadioControlItemMinWidth wMetrics) w
				return (RadioItemInfo{radioItem=(text,wOK,f)
						,radioItemSize=Size{w=wOK,h=hOK}
						,radioItemPos=zero
						,radioItemPtr=osNoWindowPtr})



instance Controls SliderControl where
	controlToHandles (SliderControl direction cWidth sliderState action atts)
		= do {
			wMetrics <- accIOEnv ioStGetOSWindowMetrics;
			size <- liftIO (getSliderSize wMetrics isHorizontal cWidth);
			return [wElementHandleToControlState
				(WItemHandle
				{	wItemId		= getIdAttribute atts
				,	wItemNr		= 0
				,	wItemKind	= IsSliderControl
				,	wItemShow	= not (any isControlHide atts)
				,	wItemSelect	= getSelectStateAttribute atts
				,	wItemInfo	= WSliderInfo (SliderInfo
								{	sliderInfoDir	= direction
								,	sliderInfoLength= (if isHorizontal then w else h) size -- PA: maybe this field is now redundant
								,	sliderInfoState	= validateSliderState sliderState
								,	sliderInfoAction= action
								})
				,	wItemAtts	= filter (not . redundantAttribute) atts
				,	wItems		= []
				,	wItemVirtual	= False
				,	wItemPos	= zero
				,	wItemSize	= size
				,	wItemPtr	= osNoWindowPtr
				,	wItemLayoutInfo	= undefined
				})
			]
		}
		where
			isHorizontal = direction == Horizontal

			getSliderSize :: OSWindowMetrics -> Bool -> ControlWidth -> IO Size
			getSliderSize wMetrics isHorizontal (PixelWidth reqW) = do
				let (wOK,hOK) = osGetSliderControlSize wMetrics isHorizontal reqW
				return (Size{w=wOK,h=hOK})
			getSliderSize wMetrics isHorizontal (TextWidth wtext) = do
				w <- getDialogFontTextWidth wMetrics wtext
				let (wOK,hOK) = osGetSliderControlSize wMetrics isHorizontal w
				return (Size{w=wOK,h=hOK})
			getSliderSize wMetrics isHorizontal (ContentWidth wtext) = do
				w <- getDialogFontTextWidth wMetrics wtext
				let (wOK,hOK) = osGetSliderControlSize wMetrics isHorizontal w
				return (Size{w=wOK,h=hOK})


instance Controls TextControl where
	controlToHandles (TextControl textLine atts)
		= do {
			wMetrics <- accIOEnv ioStGetOSWindowMetrics;
			size     <- liftIO (getTextSize wMetrics textLine cWidth);
			return
				[wElementHandleToControlState
					(WItemHandle
						{ wItemId         = getIdAttribute atts
						, wItemNr         = 0
						, wItemKind       = IsTextControl
						, wItemShow	  = not (any isControlHide atts)
						, wItemSelect	  = getSelectStateAttribute atts
						, wItemInfo       = WTextInfo (TextInfo {textInfoText=textLine})
						, wItemAtts       = filter (not . redundantAttribute) atts
						, wItems	  = []
						, wItemVirtual	  = False
						, wItemPos        = zero
						, wItemSize       = size
						, wItemPtr        = osNoWindowPtr
						, wItemLayoutInfo = undefined
						})
				]
		  }
		where
			cWidth = getControlWidthAttribute atts

			getTextSize :: OSWindowMetrics -> String -> Maybe ControlWidth -> IO Size
			getTextSize wMetrics _ (Just (PixelWidth reqW))
				= return (Size {w=wOK,h=hOK})
				where
					wOK = max (osGetTextControlMinWidth wMetrics) reqW
					hOK = osGetTextControlHeight wMetrics
			getTextSize wMetrics _ (Just (TextWidth wtext))
				= do {
					width <- getDialogFontTextWidth wMetrics wtext;
					let wOK = max (osGetTextControlMinWidth wMetrics) width
					    hOK = osGetTextControlHeight wMetrics
					in  return (Size {w=wOK,h=hOK})
				  }
			getTextSize wMetrics _ (Just (ContentWidth wtext))
				= do {
					(width,hOK) <- osGetTextControlSize wMetrics wtext;
					let wOK = max (osGetTextControlMinWidth wMetrics) width
					in  return (Size {w=wOK,h=hOK})
				  }
			getTextSize wMetrics text Nothing
				= do {
					(width,hOK) <- osGetTextControlSize wMetrics text;
					let wOK = max (osGetTextControlMinWidth wMetrics) width
					in  return (Size {w=wOK,h=hOK})
				  }

-- getDialogFontTextWidth is not the proper implementation because accScreenPicture class not yet implemented.
-- This version gives a crude estimation in terms of max width times text length.
getDialogFontTextWidth :: OSWindowMetrics -> String -> IO Int
getDialogFontTextWidth wMetrics text
	= do {
		(_,_,_,maxwidth) <- osGetFontMetrics Nothing (osmFont wMetrics);
		return ((length text) * maxwidth)
	  }

getIdAttribute :: [ControlAttribute ls ps] -> Maybe Id
getIdAttribute atts
	| hasId     = Just (getControlIdAtt idAtt)
	| otherwise = Nothing
	where
		(hasId,idAtt) = cselect isControlId undefined atts

getControlWidthAttribute :: [ControlAttribute ls ps] -> Maybe ControlWidth
getControlWidthAttribute atts
	| hasControlWidth = Just (getControlWidthAtt widthAtt)
	| otherwise       = Nothing
	where
		(hasControlWidth,widthAtt) = cselect isControlWidth undefined atts

getSelectStateAttribute :: [ControlAttribute ls ps] -> Bool
getSelectStateAttribute atts
	= enabled (getControlSelectStateAtt (snd (cselect isControlSelectState (ControlSelectState Able) atts)))

redundantAttribute :: ControlAttribute ls ps -> Bool
redundantAttribute (ControlId _) = True
redundantAttribute _             = False

getInitialPen :: [ControlAttribute ls ps] -> Pen
getInitialPen atts
    | hasPenAtts = foldr setPenAttribute defaultPen (reverse (getControlPenAtt penAttsAtt))
    | otherwise  = defaultPen
    where
	(hasPenAtts,penAttsAtt)	= cselect isControlPen undefined atts


validateLayout :: Int -> RowsOrColumns -> RowsOrColumns
validateLayout nrItems (Rows    n) = Rows    (setBetween n 1 nrItems)
validateLayout nrItems (Columns n) = Columns (setBetween n 1 nrItems)

validatePopUpIndex :: Int -> Index -> Index
validatePopUpIndex nrItems index
	| isBetween index 1 nrItems 	= index
	| otherwise			= 1

validateOrigin :: ViewDomain -> Point2 -> Point2
validateOrigin (Rectangle{corner1=Point2{x=corX1, y=corY1}, corner2=Point2{x=corX2, y=corY2}}) (Point2{x=origX, y=origY})
	= Point2 {	x=setBetween origX corX1 corX2
	  	 ,	y=setBetween origY corY1 corY2
	  	 }
