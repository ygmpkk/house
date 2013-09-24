module StdTimerDef where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	IOState defines the GUI environment types and their access functions.
--	********************************************************************************

import	StdIOCommon
import	StdGUI


data Timer t ls ps = Timer TimerInterval (t ls ps) [TimerAttribute ls ps]

type TimerInterval = Int

data TimerAttribute ls ps					-- Default:
	= TimerFunction		(TimerFunction ls ps)		-- \_ x->x
	| TimerId		Id				-- no Id
	| TimerInit		(GUIFun ls ps)			-- no actions after opening timer
	| TimerSelectState	SelectState			-- timer Able

type TimerFunction ls ps = NrOfIntervals -> GUIFun ls ps
type NrOfIntervals = Int
