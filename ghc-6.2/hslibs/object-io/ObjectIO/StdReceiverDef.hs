module StdReceiverDef (module StdReceiverDef, module StdIOCommon, module StdGUI) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdReceiverDef contains the types to define the standard set of receivers.
--	********************************************************************************


import StdGUI
import StdIOCommon


data	Receiver         m ls ps = Receiver (RId m) (ReceiverFunction m ls ps) [ReceiverAttribute ls ps]
type	ReceiverFunction m ls ps = m -> GUIFun ls ps

data	Receiver2         m r ls ps = Receiver2 (R2Id m r) (Receiver2Function m r ls ps) [ReceiverAttribute ls ps]
type	Receiver2Function m r ls ps = m -> (ls,ps) -> GUI ps (r,(ls,ps))

data	ReceiverAttribute ls ps				-- Default:
 =	ReceiverInit               (GUIFun ls ps)	-- no actions after opening receiver
 |	ReceiverSelectState        SelectState		-- receiver Able
{-	TCP support not yet incorporated
 |	ReceiverConnectedReceivers [Id]			-- [] -- MW11++
-}