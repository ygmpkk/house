module StdMenuReceiver(MenuElements(..)) where


--	Clean Object I/O library, version 1.2

import	CommonDef
import	StdIOCommon
import	StdReceiverAttribute
import	StdMenuElementClass
import	StdMenuDef
import	MenuHandle
import	ReceiverAccess(newReceiverHandle, newReceiver2Handle)


instance MenuElements (Receiver m) where
	menuElementToHandles (Receiver rid f atts) =
		return [menuElementHandleToMenuElementState
				(MenuReceiverHandle (newReceiverHandle rid (getSelectState atts) f)
						(MenuId (rIdtoId rid):map receiverAttToMenuAtt atts))
			]

instance MenuElements (Receiver2 m r) where
	menuElementToHandles (Receiver2 rid f atts) =
		return [menuElementHandleToMenuElementState
				(MenuReceiverHandle (newReceiver2Handle rid (getSelectState atts) f)
						(MenuId (r2IdtoId rid):map receiverAttToMenuAtt atts))
			]

getSelectState :: [ReceiverAttribute ls ps] -> SelectState
getSelectState rAtts = getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) rAtts))

receiverAttToMenuAtt :: ReceiverAttribute ls ps -> MenuAttribute ls ps
receiverAttToMenuAtt (ReceiverSelectState s) = MenuSelectState s
