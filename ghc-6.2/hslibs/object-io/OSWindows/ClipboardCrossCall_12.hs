module ClipboardCrossCall_12 where


import	ClCrossCall_12
import	ClCCall_12
import	Cutil_12(int2addr, addr2int)
import  Foreign.Marshal.Utils
import  Foreign.Marshal.Alloc(free)
import  Foreign.C.String


--	PA: Predefined Clipboard Formats.
cf_TEXT             =	1
cf_BITMAP           =	2
cf_METAFILEPICT     =	3
cf_SYLK             =	4
cf_DIF              =	5
cf_TIFF             =	6
cf_OEMTEXT          =	7
cf_DIB              =	8
cf_PALETTE          =	9
cf_PENDATA          =	10
cf_RIFF             =	11
cf_WAVE             =	12
cf_UNICODETEXT      =	13
cf_ENHMETAFILE      =	14
--	PA: end of addition.


winGetClipboardText :: IO String
winGetClipboardText = do
	rcci <- issueCleanRequest2 (errorCallback2 "winGetClipboardText") (rq0Cci ccRqGETCLIPBOARDTEXT)	
	(if ccMsg rcci == ccRETURN1 then do
		let ptr = (int2addr (p1 rcci))
		s <- peekCString ptr
		free ptr
		return s
	 else if ccMsg rcci == ccWASQUIT	then return ""
	 else error "[winGetClipboardText] expected ccRETURN1 value.\n")

winSetClipboardText :: String -> IO ()
winSetClipboardText text = do
	textptr <- newCString text
	issueCleanRequest2 (errorCallback2 "winSetClipboardText") (rq1Cci ccRqSETCLIPBOARDTEXT (addr2int textptr))
	free textptr

winHasClipboardText :: IO Bool
winHasClipboardText = do
	rcci <- issueCleanRequest2 (errorCallback2 "winHasClipboardText") (rq0Cci ccRqCLIPBOARDHASTEXT)
	let ok =
		if      ccMsg rcci == ccRETURN1 then toBool (p1 rcci)
	  	else if ccMsg rcci == ccWASQUIT	then False
	  	else 				error "[winHasClipboardText] expected ccRETURN1 value."
	return ok

winGetClipboardCount :: IO Int
winGetClipboardCount = do
	rcci <- issueCleanRequest2 (errorCallback2 "winGetClipboardCount") (rq0Cci ccRqGETCLIPBOARDCOUNT)
	let clipboardCount =
		if 	ccMsg rcci == ccRETURN1 then p1 rcci
	  	else if ccMsg rcci == ccWASQUIT	then 0
	  	else 				error "[winGetClipboardCount] expected ccRETURN1 value.\n"
	return clipboardCount
