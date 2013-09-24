module StdReceiverAttribute (module StdReceiverAttribute, module StdReceiverDef) where


--	********************************************************************************
--	Clean Standard Object I/O library, version 1.2
--	
--	StdReceiverAttribute specifies which ReceiverAttributes are valid for each of 
--	the standard receivers.
--	Basic comparison operations and retrieval functions are also included.
--	********************************************************************************


import StdGUI
import StdIOCommon
import StdReceiverDef


{-	The following functions specify the valid attributes for each standard receiver.
-}

isValidReceiverAttribute :: ReceiverAttribute ls ps -> Bool
isValidReceiverAttribute (ReceiverInit _)        = True
isValidReceiverAttribute (ReceiverSelectState _) = True

isValidReceiver2Attribute :: ReceiverAttribute ls ps -> Bool
isValidReceiver2Attribute (ReceiverInit _)        = True
isValidReceiver2Attribute (ReceiverSelectState _) = True


isReceiverInit :: ReceiverAttribute ls ps -> Bool
isReceiverInit (ReceiverInit _) = True
isReceiverInit _                = False

isReceiverSelectState :: ReceiverAttribute ls ps -> Bool
isReceiverSelectState (ReceiverSelectState _) = True
isReceiverSelectState _                       = False

{-	TCP support not yet incorporated
isReceiverConnectedReceivers :: ReceiverAttribute ls ps -> Bool
isReceiverConnectedReceivers (ReceiverConnectedReceivers _) = True
isReceiverConnectedReceivers _                              = False
-}

getReceiverInitFun :: ReceiverAttribute ls ps -> GUIFun ls ps
getReceiverInitFun (ReceiverInit f) = f

getReceiverSelectStateAtt :: ReceiverAttribute ls ps -> SelectState
getReceiverSelectStateAtt (ReceiverSelectState s) = s

{-	TCP support not yet incorporated
getReceiverConnectedReceivers :: ReceiverAttribute ls ps -> [Id] -- MW11++
getReceiverConnectedReceivers (ReceiverConnectedReceivers ids) = ids
-}
