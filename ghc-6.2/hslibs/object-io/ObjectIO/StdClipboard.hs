module StdClipboard where


-- Clean Object I/O library, version 1.2


import	Maybe
import	OSClipboard
import	CommonDef(dumpFatalError, remove)
import	IOState(GUI(..), ioStGetClipboardState, ioStSetClipboardState, ClipboardState(..), liftIO, accIOEnv, appIOEnv)


stdClipboardFatalError :: String -> String -> x
stdClipboardFatalError function error
	= dumpFatalError function "StdClipboard" error


--	The clipboard item type:

data ClipboardItem
	= ClipboardString String		-- Support for strings
--	| ClipboardPict	Handle			-- Support for pictures (PA: not supported yet)

class Clipboard item where
	toClipboard	:: item			-> ClipboardItem
	fromClipboard	:: ClipboardItem	-> Maybe item

instance Clipboard String where
	toClipboard string = ClipboardString string
	fromClipboard (ClipboardString string) = Just string
	


--	Reading and writing the value of the selection to the clipboard:

setClipboard :: [ClipboardItem] -> GUI ps ()
setClipboard clipItems = liftIO (mapM_ clipboardItemToScrap singleItems)
    where
	singleItems = removeDuplicateClipItems clipItems

	removeDuplicateClipItems :: [ClipboardItem] -> [ClipboardItem]
	removeDuplicateClipItems (item:items) =
	    let (_,_,items1)	= remove (eqClipboardType item) undefined items
	    in (item:removeDuplicateClipItems items1)
	    where
		eqClipboardType :: ClipboardItem -> ClipboardItem -> Bool
		eqClipboardType (ClipboardString _) item	= case item of
			(ClipboardString _)	-> True
			_			-> False
	removeDuplicateClipItems [] = []

	clipboardItemToScrap :: ClipboardItem -> IO ()
	clipboardItemToScrap (ClipboardString text) = osSetClipboardText text

getClipboard :: GUI ps [ClipboardItem]
getClipboard = do
    contents <- liftIO (osGetClipboardContent)
    let contents1 = filter ((==) osClipboardText) contents
    clipItems <- liftIO (mapM scrapToClipboardItem contents1)
    cbs <- accIOEnv (ioStGetClipboardState)
    version <- liftIO (osGetClipboardVersion)
    appIOEnv (ioStSetClipboardState cbs{cbsCount=version})
    return clipItems
    where
	scrapToClipboardItem :: Int -> IO ClipboardItem
	scrapToClipboardItem clipType
		| clipType == osClipboardText = do
			text <- osGetClipboardText
		 	return (ClipboardString text)
		| otherwise = stdClipboardFatalError "getClipboard" ("unimplemented clipboard content of type: " ++ show clipType)

clipboardHasChanged :: GUI ps Bool
clipboardHasChanged = do
	cbs <- accIOEnv (ioStGetClipboardState)
	let oldCount = cbsCount cbs
	newCount <- liftIO (osGetClipboardVersion)
	return (oldCount/=newCount)
