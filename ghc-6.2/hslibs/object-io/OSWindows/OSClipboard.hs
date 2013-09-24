module OSClipboard where

--	Clean Object I/O library, version 1.2
--	Clipboard operations.

import ClipboardCrossCall_12

type OSClipboardItemType = Int

osClipboardText = 1	-- CF_TEXT

osHasClipboardText :: IO Bool
osHasClipboardText = winHasClipboardText

osSetClipboardText :: String -> IO ()
osSetClipboardText text = winSetClipboardText text

osGetClipboardText :: IO String
osGetClipboardText = winGetClipboardText

osGetClipboardContent :: IO [OSClipboardItemType]
osGetClipboardContent = do
	hasText <- winHasClipboardText
	return (if hasText then [osClipboardText] else [])

osGetClipboardVersion :: IO Int
osGetClipboardVersion = winGetClipboardCount
