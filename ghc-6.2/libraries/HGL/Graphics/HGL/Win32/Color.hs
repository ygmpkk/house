module Graphics.HGL.Win32.Color
	( Color(..)
	, colorTable
	) where

import Graphics.HGL.Win32.Types
import Ix(Ix)
import Array(Array,array,(!))

----------------------------------------------------------------
-- The interface
----------------------------------------------------------------

data Color 
  = Black
  | Blue
  | Green 
  | Cyan
  | Red 
  | Magenta
  | Yellow
  | White
 deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

colorList  :: [(Color, RGB)]
colorTable :: Array Color RGB

----------------------------------------------------------------
-- The implementation
----------------------------------------------------------------

colorList =
  [ (Black   , RGB   0   0   0)
  , (Blue    , RGB   0   0 255)
  , (Green   , RGB   0 255   0)
  , (Cyan    , RGB   0 255 255)
  , (Red     , RGB 255   0   0)
  , (Magenta , RGB 255   0 255)
  , (Yellow  , RGB 255 255   0)
  , (White   , RGB 255 255 255)
  ]

colorTable = array (minBound, maxBound) colorList

----------------------------------------------------------------
-- The end
----------------------------------------------------------------
