module StdKey (	SpecialKey, 
		backSpaceKey, beginKey, 
		clearKey, 
		deleteKey, downKey, 
		endKey, enterKey, escapeKey, 
		f1Key,  f2Key,  f3Key,  f4Key,  f5Key,  
		f6Key,  f7Key,  f8Key,  f9Key,  f10Key,
		f11Key, f12Key, f13Key, f14Key, f15Key, 
		helpKey, 
		leftKey, 
		pgDownKey, pgUpKey, 
		rightKey, 
		upKey
              ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdKey defines the special keys for the Object I/O library. 
--	********************************************************************************


import Key hiding (toSpecialKey, isSpecialKey)
