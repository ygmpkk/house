module StdTimerAttribute( isValidTimerAttribute
			, isTimerFunction
			, isTimerId
			, isTimerInit
			, isTimerSelectState
			, getTimerFun
			, getTimerIdAtt
			, getTimerInitFun
			, getTimerSelectStateAtt
			, module StdTimerDef
			)where

--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdTimerAttribute specifies which TimerAttributes are valid for each of the
--	standard timers.
--	Basic comparison operations and retrieval functions are also included.
--	********************************************************************************

import StdGUI
import StdTimerDef
import StdIOCommon(SelectState(..))
import IOState
import Id

{-	The following functions specify the valid attributes for each standard timer. -}


isValidTimerAttribute :: TimerAttribute ls ps -> Bool
isValidTimerAttribute _ = True


isTimerFunction	:: TimerAttribute ls ps -> Bool
isTimerFunction	(TimerFunction _)	= True
isTimerFunction	_			= False

isTimerId	:: TimerAttribute ls ps -> Bool
isTimerId	(TimerId _)		= True
isTimerId	_			= False

isTimerInit	:: TimerAttribute ls ps -> Bool
isTimerInit	(TimerInit _)		= True
isTimerInit	_			= False

isTimerSelectState	:: TimerAttribute ls ps -> Bool
isTimerSelectState	(TimerSelectState _)	= True
isTimerSelectState	_			= False

getTimerFun :: TimerAttribute ls ps -> TimerFunction ls ps
getTimerFun (TimerFunction f) = f

getTimerIdAtt :: TimerAttribute ls ps -> Id
getTimerIdAtt (TimerId id) = id

getTimerInitFun :: TimerAttribute ls ps -> GUIFun ls ps
getTimerInitFun (TimerInit f) = f

getTimerSelectStateAtt :: TimerAttribute ls ps -> SelectState
getTimerSelectStateAtt (TimerSelectState s) = s
