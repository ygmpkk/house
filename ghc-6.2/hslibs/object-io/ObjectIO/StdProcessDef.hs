module StdProcessDef ( module StdProcessDef, module StdIOCommon, module StdGUI ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdProcessDef contains the types to define interactive processes.
--	********************************************************************************


import StdGUI
import StdIOCommon


data	Process
	= forall ps . Process
			DocumentInterface	-- The process document interface
			ps			-- The process private state
			(ProcessInit ps)	-- The process initialisation
			[ProcessAttribute ps]	-- The process attributes
type	ProcessInit ps
	= ps -> GUI ps ps
