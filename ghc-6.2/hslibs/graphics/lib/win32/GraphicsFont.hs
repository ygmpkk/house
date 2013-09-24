module GraphicsFont
	( Font
	, createFont, deleteFont, selectFont
	) where

import GraphicsTypes
import qualified Win32

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

newtype Font = MkFont Win32.HFONT

createFont :: Point -> Angle -> Bool -> Bool -> String -> IO Font
deleteFont :: Font -> IO ()
selectFont :: Font -> Draw Font

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

createFont (width, height) escapement bold italic family = 
 Win32.createFont (fromDimension height) (fromDimension width)
                  (round (escapement * 1800/pi))
                  0                     -- orientation
                  weight
                  italic False False    -- italic, underline, strikeout
                  Win32.aNSI_CHARSET
                  Win32.oUT_DEFAULT_PRECIS
                  Win32.cLIP_DEFAULT_PRECIS
                  Win32.dEFAULT_QUALITY
                  Win32.dEFAULT_PITCH
                  family
  >>= return . MkFont
 where
  weight | bold      = Win32.fW_BOLD
         | otherwise = Win32.fW_NORMAL

deleteFont (MkFont f) = Win32.deleteFont f

selectFont (MkFont f) = mkDraw (\hdc -> do
  f' <- Win32.selectFont hdc f
  return (MkFont f'))

----------------------------------------------------------------
-- End
----------------------------------------------------------------
