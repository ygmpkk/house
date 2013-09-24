module Graphics.HGL.Win32.Text
	( Alignment, HAlign(..), VAlign(..)
	, BkMode(..)
	, text
	, setTextColor, setBkColor, setBkMode, setTextAlignment
	) where

import qualified System.Win32 as Win32
import Array ( Ix )
import Graphics.HGL.Win32.Types
import Bits

----------------------------------------------------------------

data BkMode = Opaque | Transparent

type Alignment = (HAlign, VAlign)

-- names have a tick to distinguish them from Prelude names (blech!)
data HAlign = Left' | Center   | Right'
 deriving (Enum, Eq, Ord, Ix, Show)
data VAlign = Top   | Baseline | Bottom
 deriving (Enum, Eq, Ord, Ix, Show)

text             :: Point -> String  -> Draw ()
setTextColor     :: RGB -> Draw RGB
setBkColor       :: RGB -> Draw RGB
setBkMode        :: BkMode -> Draw BkMode
setTextAlignment :: Alignment -> Draw Alignment

----------------------------------------------------------------

type TextAlignment = Win32.TextAlignment

fromAlignment :: Alignment -> TextAlignment
fromAlignment (ha,va) = hAlign ha .|. vAlign va

hAlign :: HAlign -> TextAlignment
hAlign Left'    = Win32.tA_LEFT       
hAlign Center  	= Win32.tA_CENTER     
hAlign Right'   = Win32.tA_RIGHT      
               
vAlign :: VAlign -> TextAlignment
vAlign Top     	= Win32.tA_TOP        
vAlign Baseline	= Win32.tA_BASELINE   
vAlign Bottom  	= Win32.tA_BOTTOM     

toAlignment :: TextAlignment -> Alignment
toAlignment x = (toHAlign (x .&. hmask), toVAlign (x .&. vmask))

toHAlign x
  | x == Win32.tA_LEFT   = Left'
  | x == Win32.tA_CENTER = Center
  | x == Win32.tA_RIGHT  = Right'
  | otherwise            = Center -- safe(?) default

toVAlign x
  | x == Win32.tA_TOP      = Top
  | x == Win32.tA_BASELINE = Baseline
  | x == Win32.tA_BOTTOM   = Bottom
  | otherwise              = Baseline -- safe(?) default

-- Win32 doesn't seem to provide the masks I need - these ought to work.
hmask = Win32.tA_LEFT .|. Win32.tA_CENTER   .|. Win32.tA_RIGHT  
vmask = Win32.tA_TOP  .|. Win32.tA_BASELINE .|. Win32.tA_BOTTOM

fromBkMode :: BkMode -> Win32.BackgroundMode
fromBkMode Opaque      = Win32.oPAQUE
fromBkMode Transparent = Win32.tRANSPARENT

toBkMode :: Win32.BackgroundMode -> BkMode
toBkMode x
  | x == Win32.oPAQUE      = Opaque
  | x == Win32.tRANSPARENT = Transparent

-- ToDo: add an update mode for these constants
-- (not required at the moment since we always specify exactly where
-- the text is to go)
-- tA_NOUPDATECP :: TextAlignment
-- tA_UPDATECP   :: TextAlignment

text (x,y) s = mkDraw $ \ hdc -> 
  Win32.textOut hdc (fromDimension x) (fromDimension y) s

setTextColor c = mkDraw (\hdc -> do
  c' <- Win32.setTextColor hdc (fromRGB c)
  return (toRGB c'))
  
setBkColor c = mkDraw (\hdc -> do
  c' <- Win32.setBkColor hdc (fromRGB c)
  return (toRGB c'))
  
setBkMode m = mkDraw (\hdc -> do
  m' <- Win32.setBkMode hdc (fromBkMode m)
  return (toBkMode m'))
  
setTextAlignment new_alignment = mkDraw (\hdc -> do
  old <- Win32.setTextAlign hdc (fromAlignment new_alignment)
  return (toAlignment old)
  )

----------------------------------------------------------------
