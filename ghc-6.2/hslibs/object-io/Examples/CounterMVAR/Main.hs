module Main (main) where

import CleanStdMisc
import CounterControl
import StdIO

main
	= startIO SDI init [ProcessClose closeProcess]
	where
		init = do {
				error <- openDialog ddef;
				if   error/=NoError
				then abort "Main could not open Dialog."
				else return ()
			}
		ddef = Dialog "Counter Dialog"
			(   CounterControl 5
			)   [WindowClose closeProcess]









-- Testing
f :: (Int -> Int -> Int) -> MVar Int -> GUI ()
f op c = do count <- liftIO (takeMVar c)
            let newcount = count `op` 1
            setControlText displayId (show newcount)
            liftIO (putMVar c newcount)

input :: KeyboardState -> GUI ()
input _
    = do Just window <- getParentWindow inId
         let text     = fromJust (snd (getControlText inId window))
         error       <- asyncSend you (NewLine text)
         return ()
    where
    	inId = error "Id"
    	you  = error "RId"
data Message = NewLine String | Quit
displayId :: Id
displayId = error "Id"
data MVar a = MVar a
newMVar :: a -> IO (MVar a)
newMVar a = return (MVar a)
takeMVar :: MVar a -> IO a
takeMVar (MVar a) = return a
putMVar :: MVar a -> a -> IO ()
putMVar _ _ = return ()
