module OSFont ( Font, OSFont(..), OSFontDef
              , osSelectFont, osDefaultFont, osDialogFont
              , osFontGetDef, osFontGetImp
              , osFontNames, osFontStyles, osFontSizes
              , osGetFontCharWidths, osGetFontStringWidths, osGetFontMetrics
              ) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	OSFont defines all font related functions and data types.
--	********************************************************************************


import Cutil_12
import ClCCall_12(winGetVertResolution)
import ClCrossCall_12
import CommonDef(dumpFatalError, isBetween, minmax)
import PictCCall_12


data	Font
	= Font
		{ fontdef      :: !OSFontDef	-- The font requested by the program
		, fontimp      :: !OSFont	-- The font selected  by the system
		}
data	OSFont
	= OSFont
		{ osfontname   :: !String	-- Name of the font
		, osfontstyles :: !Int		-- Style variations of the font
		, osfontsize   :: !Int		-- Size of the font
		}
	deriving(Eq)
type	OSFontDef
	=	( String			-- Name of the font
		, [String]			-- Style variations of the font
		, Int				-- Point size of the font
		)

{-	NOTE: These local definitions MUST be the same as in StdPictureDef!
	      They are not imported to circumvent cyclic dependency.
-}
os_italicsStyle     = "Italic"
os_boldStyle        = "Bold"
os_underlinedStyle  = "Underline"


osSelectFont :: OSFontDef -> IO (Maybe Font)
osSelectFont fdef@(fName,fStyles,fSize) = return (Just (Font {fontdef=fdef,fontimp=fimp}))
	where fimp = OSFont {osfontname=fName,osfontstyles=sStyle2IStyle fStyles,osfontsize=fSize}

osDefaultFont :: IO Font
osDefaultFont = return (Font {fontdef=def,fontimp=imp})
	where
		def     = (name,styles,size)
		imp     = OSFont {osfontname=name,osfontstyles=sStyle2IStyle styles,osfontsize=size}
		name    = "Times"
		styles  = []
		size    = 10

osDialogFont :: IO Font
osDialogFont
	= return (Font {fontdef=def,fontimp=imp})
	where
		def     = (name,styles,size)
		imp     = OSFont {osfontname=name,osfontstyles=sStyle2IStyle styles,osfontsize=size}
		name    = "MS Sans Serif"
		styles  = []
		size    = 8

osFontGetDef :: Font -> OSFontDef
osFontGetDef font = fontdef font

osFontGetImp :: Font -> OSFont
osFontGetImp font = fontimp font

sStyle2IStyle :: [String] -> Int
sStyle2IStyle styles
	= fromIntegral (s2i styles 0)
	where
		s2i :: [String] -> Int32 -> Int32
		s2i (style:rest) i
			| style==os_boldStyle       = s2i rest (i .|. (fromIntegral iBold))
			| style==os_italicsStyle    = s2i rest (i .|. (fromIntegral iItalic))
			| style==os_underlinedStyle = s2i rest (i .|. (fromIntegral iUnderline))
			| otherwise                 = s2i rest i
		s2i [] i
			= i

iStyle2SStyle :: Int -> [String]
iStyle2SStyle istyle
	= idtofontstyles' istyle [iBold,iItalic,iUnderline,iStrikeOut]
	where
		idtofontstyles' :: Int -> [Int] -> [String]
		idtofontstyles' 0 _
			= []
		idtofontstyles' istyle (styleflag:styleflags)
			| notStyleFlag = styles
			| otherwise    = style:styles
			where
				notStyleFlag = (((fromIntegral istyle) .&. (fromIntegral styleflag)) :: Int32) == 0
				styles       = idtofontstyles' (istyle-styleflag) styleflags
				style        = if      (styleflag==iBold)      then os_boldStyle
				               else if (styleflag==iItalic)    then os_italicsStyle
				               else if (styleflag==iUnderline) then os_underlinedStyle
				                                               else dumpFatalError "iStyle2SStyle"
				                                                                   "Osfont"
				                                                                   ("unmatched styleflag value ("++show styleflag++")")
		idtofontstyles' _ _
			= []


osFontNames :: IO [String]
osFontNames
	= do {
		(_,unsortednames) <- issueCleanRequest fontnamesCallback (rq0Cci ccRqGETFONTNAMES) [];
		return (sortAndRemoveDuplicates unsortednames)
	  }
	where
		fontnamesCallback :: CrossCallInfo -> [String] -> IO (CrossCallInfo,[String])
		fontnamesCallback cci names
			= do {
				newname <- peekCString (int2addr (p1 cci));
				return (return0Cci,newname:names)
			  }

sortAndRemoveDuplicates :: (Ord a) => [a] -> [a]
sortAndRemoveDuplicates (e:es)
	= insert e (sortAndRemoveDuplicates es)
	where
		insert :: (Ord a) => a -> [a] -> [a]
		insert a list@(b:x)
			| a<b       = a:list
			| a>b       = b:(insert a x)
			| otherwise = list
		insert a _
			= [a]
sortAndRemoveDuplicates _
	= []


osFontStyles :: String -> IO [String]
osFontStyles fname
	= return [os_boldStyle,os_italicsStyle,os_underlinedStyle]

osFontSizes :: Int -> Int -> String -> IO [Int]
osFontSizes between1 between2 fname
	= do {
		textptr           <- newCString fname;
		(_,unsortedsizes) <- issueCleanRequest fontSizesCallback (rq1Cci ccRqGETFONTSIZES (addr2int textptr)) [];
		return (sortAndRemoveDuplicates unsortedsizes)
	  }
	where
		(low,high)         = minmax between1 between2
		
		fontSizesCallback :: CrossCallInfo -> [Int] -> IO (CrossCallInfo,[Int])
		fontSizesCallback (CrossCallInfo {p1=size,p2=0}) sizes
			= return (return0Cci,newsizes)
			where
				pts      = height2Points size
				newsizes = if   isBetween pts low high
				           then pts:sizes
				           else sizes
		fontSizesCallback _ _
			= return (return0Cci,[low..high])

height2Points :: Int -> Int
height2Points h
	= round points
	where
		dpi      = fromIntegral winGetVertResolution
		phfactor = dpi / 72.0
		points   = (fromIntegral h) / phfactor


fn Nothing  = (False, nullPtr)
fn (Just x) = (True,  x)

osGetFontCharWidths :: Maybe HDC -> [Char] -> Font -> IO [Int]
osGetFontCharWidths maybeHdc chars (Font {fontimp=OSFont {osfontname=osfontname,osfontstyles=osfontstyles,osfontsize=osfontsize}})
	= sequence [winGetCharWidth c (osfontname,osfontstyles,osfontsize) hdcPassed hdc | c <- chars]
	where
	   (hdcPassed, hdc) = fn maybeHdc

osGetFontStringWidths :: Maybe HDC -> [String] -> Font -> IO [Int]
osGetFontStringWidths maybeHdc strings (Font {fontimp=OSFont {osfontname=osfontname,osfontstyles=osfontstyles,osfontsize=osfontsize}})
	= sequence [winGetStringWidth s (osfontname,osfontstyles,osfontsize) hdcPassed hdc | s <- strings]
	where
	   (hdcPassed, hdc) = fn maybeHdc

osGetFontMetrics :: Maybe HDC -> Font -> IO (Int,Int,Int,Int)
osGetFontMetrics maybeHdc (Font {fontimp=OSFont {osfontname=osfontname,osfontstyles=osfontstyles,osfontsize=osfontsize}})
	= do {
		(ascent,descent,maxwidth,leading) <- winGetFontInfo (osfontname,osfontstyles,osfontsize) hdcPassed hdc;
		return (ascent,descent,leading,maxwidth)
	  }
	where
	   (hdcPassed, hdc) = fn maybeHdc
