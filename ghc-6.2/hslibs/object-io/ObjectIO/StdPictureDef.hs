module StdPictureDef(module StdPictureDef, Font) where


--	********************************************************************************
--	Clean to Haskell Standard Object I/O library, version 1.2
--	
--	StdPictureDef contains the predefined figures that can be drawn.
--	********************************************************************************


import StdIOBasic
import OSFont (Font)


data	Line2						-- A line connects two points
	= Line2
		{ line_end1       :: !Point2		-- The first  point
		, line_end2       :: !Point2		-- The second point
		}
data	Box						-- A box is a rectangle
	= Box
		{ box_w           :: !Int		-- The width  of the box
		, box_h           :: !Int		-- The height of the box
		}
data	Oval						-- An oval is a stretched unit circle
	= Oval
		{ oval_rx         :: !Int		-- The horizontal radius (stretch)
		, oval_ry         :: !Int		-- The vertical   radius (stretch)
		}
data	Curve						-- A curve is a slice of an oval
	= Curve
		{ curve_oval      :: !Oval		-- The source oval
		, curve_from      :: !Float		-- Starting angle (in radians)
		, curve_to        :: !Float		-- Ending   angle (in radians)
		, curve_clockwise :: !Bool		-- Direction: True iff clockwise
		}
data	Polygon						-- A polygon is an outline shape
	= Polygon
		{ polygon_shape   :: ![Vector2]		-- The shape of the polygon
		}
data	FontDef
	= FontDef
		{ fName           :: !FontName		-- Name of the font
		, fStyles         :: ![FontStyle]	-- Stylistic variations
		, fSize           :: !FontSize		-- Size in points
		}
data	FontMetrics
	= FontMetrics
		{ fAscent         :: !Int		-- Distance between top    and base line
		, fDescent        :: !Int		-- Distance between bottom and base line
		, fLeading        :: !Int		-- Distance between two text lines
		, fMaxWidth       :: !Int		-- Max character width including spacing
		}
		
type	FontName  = String
type	FontStyle = String
type	FontSize  = Int

data	Colour
	= RGB 
	    { r	:: !Int				-- The contribution of red
	    , g	:: !Int				-- The contribution of green
	    , b	:: !Int				-- The contribution of blue
	    }
	| Black    | White
	| DarkGrey | Grey    | LightGrey		-- 75%, 50%, and 25% Black
	| Red      | Green   | Blue
	| Cyan     | Magenta | Yellow

data	PenAttribute					-- Default:
	= PenSize	Int				-- 1
	| PenPos	Point2				-- zero
	| PenColour	Colour				-- Black
	| PenBack	Colour				-- White
	| PenFont	Font				-- defaultFont

--	Colour constants:
minRGB			= 0   :: Int
maxRGB			= 255 :: Int

--	Font constants:
serifFontDef		= FontDef {fName="Times New Roman",fStyles=[],fSize=10}
sansSerifFontDef	= FontDef {fName="Arial",          fStyles=[],fSize=10}
smallFontDef		= FontDef {fName="Small Fonts",    fStyles=[],fSize=7 }
nonProportionalFontDef	= FontDef {fName="Courier New",    fStyles=[],fSize=10}
symbolFontDef		= FontDef {fName="Symbol",         fStyles=[],fSize=10}

--	Font style constants:
italicsStyle		= "Italic"
boldStyle		= "Bold"
underlinedStyle		= "Underline"


--	Standard lineheight of a font is the sum of its leading, ascent and descent:
fontLineHeight fMetrics	= fLeading fMetrics + fAscent fMetrics + fDescent fMetrics
