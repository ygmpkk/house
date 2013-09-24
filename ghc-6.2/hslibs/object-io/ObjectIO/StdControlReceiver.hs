module StdControlReceiver where


--	Clean Object I/O library, version 1.2


import	StdControlClass
import	StdReceiverAttribute
import	WindowHandle
import	CommonDef(cselect)
import	ReceiverAccess(newReceiverHandle,newReceiver2Handle)
import	OSTypes(osNoWindowPtr)


instance Controls (Receiver m) where	
	controlToHandles (Receiver rid f atts) =
		return [wElementHandleToControlState
				(WItemHandle 
					{ wItemId		= Just (rIdtoId rid)
					, wItemNr		= 0
					, wItemKind		= IsReceiverControl
					, wItemShow		= False
					, wItemSelect		= enabled select
					, wItemInfo		= WReceiverInfo (newReceiverHandle rid select f)
					, wItemAtts		= []
					, wItems		= []
					, wItemVirtual		= True
					, wItemPos		= zero
					, wItemSize		= zero
					, wItemPtr		= osNoWindowPtr
					, wItemLayoutInfo	= LayoutFix
					})
			]		  
		where		
			select	= getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) atts))


instance Controls (Receiver2 m r) where	
	controlToHandles (Receiver2 rid f atts) =
		return [wElementHandleToControlState
				(WItemHandle 
					{ wItemId		= Just (r2IdtoId rid)
					, wItemNr		= 0
					, wItemKind		= IsReceiverControl
					, wItemShow		= False
					, wItemSelect		= enabled select
					, wItemInfo		= WReceiverInfo (newReceiver2Handle rid select f)
					, wItemAtts		= []
					, wItems		= []
					, wItemVirtual		= True
					, wItemPos		= zero
					, wItemSize		= zero
					, wItemPtr		= osNoWindowPtr
					, wItemLayoutInfo	= LayoutFix
					})
			]		  
		where		
			select	= getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) atts))