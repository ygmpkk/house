-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.X11.Font
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

module Graphics.HGL.X11.Font
	( createFont
	, deleteFont
	) where

import qualified Graphics.X11.Xlib as X
import Graphics.HGL.X11.DC
import qualified Graphics.HGL.X11.Utilities as Utils

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
