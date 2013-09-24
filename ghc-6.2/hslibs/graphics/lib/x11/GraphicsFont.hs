module GraphicsFont
	( createFont
	, deleteFont
	) where

import qualified Xlib as X
import GraphicsDC
import qualified Graphics_Utilities as Utils
import Concurrent( readMVar )

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

createFont :: Point -> Angle -> Bool -> Bool -> String -> IO Font
deleteFont :: Font -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

createFont (width, height) escapement bold italic family = do
  display <- getDisplay
--  print fontName
  r <- Utils.safeTry (X.loadQueryFont display fontName)
  case r of
    Left e  -> ioError (userError $ "Unable to load font " ++ fontName)
    Right f -> return f
 where
  fontName = concatMap ('-':) fontParts
  fontParts = [ foundry
              , family
              , weight
              , slant
              , sWdth
              , adstyl
              , pxlsz
              , ptSz
              , resx
              , resy
              , spc
              , avgWidth
              , registry
              , encoding
              ]
  foundry  = "*" -- eg "adobe"
  -- family   = "*" -- eg "courier"
  weight   = if bold then "bold" else "medium"
  slant    = if italic then "i" else "r"
  sWdth    = "normal"
  adstyl   = "*"
  pxlsz    = show height
  ptSz     = "*"
  resx     = "75"
  resy     = "75"
  spc      = "*"
  avgWidth = show (width*10) -- not sure what unit they use
  registry = "*"
  encoding = "*"
  
  
deleteFont f = do
  display <- getDisplay
  X.freeFont display f

----------------------------------------------------------------
-- End
----------------------------------------------------------------
