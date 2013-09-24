module StdId ( Ids(..)
	     , openIds, openRIds, openR2Ids
             , getParentId
             , Id.Id
             , IOState.GUI, IOState.IOSt
             ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdId specifies the generation functions for identification values.
--	********************************************************************************


import Id
import IOState
import Maybe
import World
import Data.IORef


class Monad envM => Ids envM where
	openId    :: envM Id
	openR2Id  :: envM (R2Id m r)
	
	openRId  :: envM (RId m)	

instance Ids IO where
	openId  = do
		i <- loadWorld
		storeWorld (i-1)
		return (toId i)
	
	openR2Id = do
		i   <- loadWorld
		cIn  <- newIORef undefined
		cOut <- newIORef undefined
		storeWorld (i-1)
		return (toR2Id i cIn cOut)
		
	openRId = do
		i   <- loadWorld
		cIn  <- newIORef []
		storeWorld (i-1)
		return (toRId i cIn)

instance Ids (GUI ps) where
	openId  = do
		i <- ioStGetIdSeed
		ioStSetIdSeed (i-1)
		return (toId i)
	
	openR2Id = do
		i   <- ioStGetIdSeed
		cIn  <- liftIO (newIORef undefined)
		cOut <- liftIO (newIORef undefined)
		ioStSetIdSeed (i-1)
		return (toR2Id i cIn cOut)
		
	openRId = do
		i   <- ioStGetIdSeed
		cIn  <- liftIO (newIORef [])
		ioStSetIdSeed (i-1)
		return (toRId i cIn)


openIds  :: Ids envM => Int -> envM [Id]
openIds n = sequence (replicate n openId)

openR2Ids :: Ids envM => Int -> envM [R2Id m r]
openR2Ids n = sequence (replicate n openR2Id)

openRIds :: Ids envM => Int -> envM [RId m]
openRIds n = sequence (replicate n openRId)

getParentId :: Id -> GUI ps (Maybe Id)
getParentId id
	= ioStGetIdTable >>= (\idtable -> return (fmap idpId (getIdParent id idtable)))
