module StdTimer(module StdTimer, module	StdTimerDef) where


--	Clean Object I/O library, version 1.2

import	CommonDef
import  Id
import  IOState
import  TimerAccess
import  TimerDefAccess
import  TimerDevice
import  TimerTable
import  TimerHandle
import	StdId
import	StdTimerDef
import	StdTimerAttribute
import	StdTimerElementClass
import  Control.Monad(when)
import  System.IO(fixIO)

stdTimerFatalError :: String -> String -> x
stdTimerFatalError function error
	= dumpFatalError function "StdTimer" error


--	Open timer:

class Timers tdef where
	openTimer :: ls -> tdef ls ps -> ps -> GUI ps ps
	

instance TimerElements t => Timers (Timer t) where	
	openTimer ls tDef@(Timer period items atts) ps = do
		ps <- dOpen timerFunctions ps
		id <- validateTimerId maybeId
		(ok,timers) <- ioStGetTimerHandles
		when (not ok) (stdTimerFatalError "openTimer (Timer)" "could not retrieve TimerSystemState from IOSt")
		pid <- accIOEnv ioStGetIOId
		it <- ioStGetIdTable
		rt <- ioStGetReceiverTable
		tt <- ioStGetTimerTable
		ts <- timerElementToHandles items
		let itemHs = map timerElementStateToTimerElementHandle ts
		let (ok,rt1,it1) = bindTimerElementIds pid id itemHs rt it
		(if not ok
		 then do
			appIOEnv (ioStSetDevice (TimerSystemState timers))
			throwGUI ErrorIdsInUse
		 else do
		 	let tLoc = TimerLoc
				{ tlIOId	= pid
				, tlDevice	= TimerDevice
				, tlParentId	= id
				, tlTimerId	= id
				}
			let tt1	= if ableTimer then (snd (addTimerToTimerTable tLoc period tt)) else tt
			let it2	= snd (addIdToIdTable id IdParent{idpIOId=pid,idpDevice=TimerDevice,idpId=id} it1)
			ioStSetTimerTable tt1
			ioStSetReceiverTable rt1
			ioStSetIdTable it2
			let tH = TimerHandle
				{ tId		= id
				, tSelect	= ableTimer
				, tPeriod	= max 0 period
				, tFun		= f
				, tItems	= itemHs
				}
			(_,ps) <- toGUI (\ioState -> fixIO(\st -> fromGUI (timerInit (ls,ps))
					(ioStSetDevice 
						(TimerSystemState 
							timers{tTimers=(TimerStateHandle (TimerLSHandle {tState=fst (fst st),tHandle=tH}):tTimers timers)})
						ioState)))
			return ps)
		where
			(hasIdAtt,idAtt)	= cselect isTimerId undefined atts
			maybeId			= if hasIdAtt then Just (getTimerIdAtt idAtt) else Nothing
			ableTimer		= enabled (getTimerSelectStateAtt (snd (cselect isTimerSelectState (TimerSelectState Able) atts)))
			f			= getTimerFun (snd (cselect isTimerFunction (TimerFunction (\_ st->return st)) atts))
			timerInit		= getTimerInitFun (snd (cselect isTimerInit (TimerInit return) atts))
			

			validateTimerId :: Maybe Id -> GUI ps Id
			validateTimerId Nothing = openId
			validateTimerId (Just id) = do
				it <- ioStGetIdTable
				(if memberIdTable id it then throwGUI ErrorIdsInUse
				 else return id)
	
	

eqTimerStateHandleId :: Id -> TimerStateHandle ps -> Bool
eqTimerStateHandleId id (TimerStateHandle (TimerLSHandle {tHandle=tH})) = id == tId tH


--	Close timer:

closeTimer :: Id -> GUI ps ()
closeTimer id = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return ()
	 else do
		pid <- accIOEnv ioStGetIOId
		rt <- ioStGetReceiverTable
		tt <- ioStGetTimerTable
		it <- ioStGetIdTable
	  	let (rt1,tt1,it1) = closeTimer' id pid rt tt it (tTimers tHs)
		ioStSetIdTable it1
		ioStSetReceiverTable rt1
		ioStSetTimerTable tt1	  
		appIOEnv (ioStSetDevice (TimerSystemState tHs)))
	where
		closeTimer' :: Id -> SystemId -> ReceiverTable -> TimerTable -> IdTable -> [TimerStateHandle ps] -> (ReceiverTable,TimerTable,IdTable)
		closeTimer' id pid rt tt it (tsH:tsHs)		
			| eqTimerStateHandleId id tsH = disposeElementIds pid tsH tt rt it
			| otherwise = closeTimer' id pid rt tt it tsHs
			where
				disposeElementIds :: SystemId -> TimerStateHandle ps -> TimerTable -> ReceiverTable -> IdTable -> (ReceiverTable,TimerTable,IdTable)
				disposeElementIds pid (TimerStateHandle (TimerLSHandle {tHandle=tH})) tt rt it =
					let (tt1,rt1,it1) = unbindTimerElementIds pid (tItems tH) (tt,rt,it)
					in (rt1,snd (removeTimerFromTimerTable teLoc tt1),snd (removeIdFromIdTable (tId tH) it1))
					where
						teLoc	= TimerLoc {tlIOId=pid,tlDevice=TimerDevice,tlParentId=(tId tH),tlTimerId=(tId tH)}
		closeTimer' _ _ rt tt it [] = (rt,tt,it)


--	Get the Ids and TimerTypes of all timers:

getTimers :: GUI ps [Id]
getTimers = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return []
	 else do
		let ids	= getIds (tTimers tHs)	  
		appIOEnv (ioStSetDevice (TimerSystemState tHs))
		return ids)
	where
		getIds :: [TimerStateHandle ps] -> [Id]
		getIds [] = []
		getIds (TimerStateHandle (TimerLSHandle {tHandle=tH}):tsHs) = (tId tH : getIds tsHs)
	
	

--	Enabling and Disabling of timers:

enableTimer :: Id -> GUI ps ()
enableTimer id = changeTimer id enableTimer'
	where
		enableTimer' :: TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable, TimerStateHandle ps)
		enableTimer' teLoc tt (TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH}))
			| tSelect tH = (tt,TimerStateHandle tlsH)
			| otherwise  =
				let (_,tt1) = addTimerToTimerTable teLoc (tPeriod tH) tt
				in (tt1,TimerStateHandle tlsH{tHandle=tH{tSelect=True}})

disableTimer :: Id -> GUI ps ()
disableTimer id = changeTimer id disableTimer'
	where
		disableTimer' :: TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable, TimerStateHandle ps)
		disableTimer' teLoc tt (TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH}))
			| not (tSelect tH) = (tt,TimerStateHandle tlsH)
			| otherwise =
				let (_,tt1) = removeTimerFromTimerTable teLoc tt
				in (tt1,TimerStateHandle tlsH{tHandle=tH{tSelect=False}})


--	Get the SelectState of timers:

getTimerSelectState :: Id -> GUI ps (Maybe SelectState)
getTimerSelectState id = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return Nothing
	 else do
		let maybe_select = getTimerSelect id (tTimers tHs)
		appIOEnv (ioStSetDevice (TimerSystemState tHs))
		return maybe_select)
	where
		getTimerSelect :: Id -> [TimerStateHandle ps] -> Maybe SelectState
		getTimerSelect id ((TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH})):tsHs)
			| id == tId tH = Just (if tSelect tH then Able else Unable)
			| otherwise    = getTimerSelect id tsHs
		getTimerSelect _ [] = Nothing


--	Set the TimerInterval of timers:

setTimerInterval :: Id -> TimerInterval -> GUI ps ()
setTimerInterval id interval = do
	changeTimer id (setTimerInterval' interval)
	where
		setTimerInterval' :: TimerInterval -> TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable, TimerStateHandle ps)
		setTimerInterval' period teLoc tt tsH@(TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH}))
			| period' == tPeriod tH = (tt,tsH)
			| not (tSelect tH)	= (tt,tsH')
			| otherwise =
				let (_,tt) = setIntervalInTimerTable teLoc period' tt
				in (tt,tsH')
			where
				period'	= max 0 period
				tsH' = TimerStateHandle tlsH{tHandle=tH{tPeriod=period'}}


-- Get the TimerInterval of timers:

getTimerInterval :: Id -> GUI ps (Maybe TimerInterval)
getTimerInterval id = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok then return Nothing
	 else do
		let optInterval = getTimerInterval' id (tTimers tHs)	  
		appIOEnv (ioStSetDevice (TimerSystemState tHs))
		return optInterval)
	where
		getTimerInterval' :: Id -> [TimerStateHandle ps] -> Maybe TimerInterval
		getTimerInterval' id ((TimerStateHandle tlsH@(TimerLSHandle {tHandle=tH})):tsHs)
			| id == tId tH = Just (tPeriod tH)
			| otherwise    = getTimerInterval' id tsHs
		getTimerInterval' _ _  = Nothing


ioStGetTimerHandles :: GUI ps (Bool,TimerHandles ps)
ioStGetTimerHandles = do
	(found,tDevice) <- accIOEnv (ioStGetDevice TimerDevice)
	(if not found then return (False,undefined)
	 else return (True,timerSystemStateGetTimerHandles tDevice))


--	General TimerHandle changing function:

type DeltaTimerStateHandle ps = TimerLoc -> TimerTable -> TimerStateHandle ps -> (TimerTable,TimerStateHandle ps)

changeTimer :: Id -> DeltaTimerStateHandle ps -> GUI ps ()
changeTimer id f = do
	(ok,tHs) <- ioStGetTimerHandles
	(if not ok
	 then return ()
	 else do
		tt <- ioStGetTimerTable
		ioid <- accIOEnv ioStGetIOId
		let (tt1,tHs1) = changeTimerDevice ioid id f tt tHs
		appIOEnv (ioStSetDevice (TimerSystemState tHs1))
		ioStSetTimerTable tt1
		return ())
	where
		changeTimerDevice :: SystemId -> Id -> DeltaTimerStateHandle ps -> TimerTable -> TimerHandles ps -> (TimerTable,TimerHandles ps)
		changeTimerDevice ioid id f tt timers@(TimerHandles {tTimers=tsHs}) =
			let (tt1,tsHs1)	= changeTimerStateHandles ioid id f tt tsHs
			in (tt1,timers{tTimers=tsHs1})
			where
				changeTimerStateHandles :: SystemId -> Id -> DeltaTimerStateHandle ps -> TimerTable -> [TimerStateHandle ps] -> (TimerTable,[TimerStateHandle ps])
				changeTimerStateHandles ioid id f tt (tsH@(TimerStateHandle (TimerLSHandle {tHandle=tH})):tsHs)
					| id == tId tH =
						let
							teLoc		= TimerLoc{tlIOId=ioid,tlDevice=TimerDevice,tlParentId=id,tlTimerId=id}
							(tt1,tsH1)	= f teLoc tt tsH
						in
							 (tt1,tsH1:tsHs)
					| otherwise =
						let
							(tt1,tsHs1)	= changeTimerStateHandles ioid id f tt tsHs
						in 
							(tt1,tsH:tsHs1)
				changeTimerStateHandles _ _ _ tt tsHs = (tt,tsHs)
