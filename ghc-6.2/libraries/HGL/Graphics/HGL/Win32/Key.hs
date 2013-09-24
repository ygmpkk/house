module Graphics.HGL.Win32.Key (
    Key (MkKey),
    keyToChar,		-- :: Key -> Char 
    isCharKey,		-- :: Key -> Bool
    isBackSpaceKey,	-- :: Key -> Bool
    isTabKey,		-- :: Key -> Bool
--    isLineFeedKey, 	-- :: Key -> Bool
    isClearKey, 	-- :: Key -> Bool
    isReturnKey, 	-- :: Key -> Bool
    isEscapeKey,	-- :: Key -> Bool
    isDeleteKey, 	-- :: Key -> Bool
--    isMultiKeyKey, 	-- :: Key -> Bool
    isHomeKey,		-- :: Key -> Bool
    isLeftKey,		-- :: Key -> Bool
    isUpKey,		-- :: Key -> Bool
    isRightKey,	        -- :: Key -> Bool
    isDownKey,		-- :: Key -> Bool
    isPriorKey,	        -- :: Key -> Bool
    isPageUpKey,	-- :: Key -> Bool
    isNextKey,		-- :: Key -> Bool
    isPageDownKey,	-- :: Key -> Bool
    isEndKey,		-- :: Key -> Bool
--    isBeginKey,	        -- :: Key -> Bool
    isShiftLKey,	-- :: Key -> Bool
    isShiftRKey,	-- :: Key -> Bool
    isControlLKey,	-- :: Key -> Bool
    isControlRKey,	-- :: Key -> Bool
--    isCapsLockKey,	-- :: Key -> Bool
--    isShiftLockKey,	-- :: Key -> Bool
--    isMetaLKey,	        -- :: Key -> Bool
--    isMetaRKey,	        -- :: Key -> Bool
--    isAltLKey,		-- :: Key -> Bool
--    isAltRKey,		-- :: Key -> Bool
) where

import Maybe (isJust)
import System.Win32

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

newtype Key = MkKey VKey deriving Show

keyToChar          :: Key -> Char 
isCharKey          :: Key -> Bool -- Is it a "real" character?
isBackSpaceKey     :: Key -> Bool
isTabKey           :: Key -> Bool
--isLineFeedKey      :: Key -> Bool
isClearKey         :: Key -> Bool
isReturnKey        :: Key -> Bool
isEscapeKey        :: Key -> Bool
isDeleteKey        :: Key -> Bool
--isMultiKeyKey      :: Key -> Bool -- Multi-key character compose.
isHomeKey          :: Key -> Bool -- Cursor home.
isLeftKey          :: Key -> Bool -- Cursor left, left arrow.
isUpKey            :: Key -> Bool -- Cursor up, up arrow.
isRightKey         :: Key -> Bool -- Cursor right, right arrow.
isDownKey          :: Key -> Bool -- Cursor down, down arrow.
isPriorKey         :: Key -> Bool -- Prior, previous page. Same as page up.
isPageUpKey        :: Key -> Bool -- Page up, previous page. Same as prior.
isNextKey          :: Key -> Bool -- Next, next page. Same as page down.
isPageDownKey      :: Key -> Bool -- Page down, next page. Same as next.
isEndKey           :: Key -> Bool -- End of line.
--isBeginKey         :: Key -> Bool -- Beginning of line.
isShiftLKey        :: Key -> Bool -- Left shift.
isShiftRKey        :: Key -> Bool -- Right shift.
isControlLKey      :: Key -> Bool -- Left control.
isControlRKey      :: Key -> Bool -- Right control.
--isCapsLockKey      :: Key -> Bool -- Caps lock.
--isShiftLockKey     :: Key -> Bool -- Shift lock.
--isMetaLKey         :: Key -> Bool -- Left meta.
--isMetaRKey         :: Key -> Bool -- Right meta.
--isAltLKey          :: Key -> Bool -- Left alt.
--isAltRKey          :: Key -> Bool -- Right alt.

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

-- Converts a VKey representing an ISO 8859-1 (Latin 1) character or one
-- of a few control characters to a Char.
-- Note! It is assumed that the VKey encoding for Latin 1 characters agrees
-- with the Haskell character encoding!
keySymToChar :: VKey -> Maybe Char
keySymToChar ks
    | space <= ks && ks <= ydiaresis = Just (toEnum (fromIntegral ks))
    | ks == vK_BACK                  = Just '\BS'
    | ks == vK_TAB                   = Just '\HT'
--    | ks == vK_LINEFEED              = Just '\LF'
    | ks == vK_CLEAR                 = Just '\FF'
    | ks == vK_RETURN                = Just '\CR'
    | ks == vK_ESCAPE                = Just '\ESC'
    | ks == vK_DELETE                = Just '\DEL'
    | otherwise                      = Nothing    
 where
  space, ydiaresis :: VKey
  space     = fromIntegral (fromEnum ' ')
  ydiaresis = fromIntegral 255        -- is this right?

-- Converts a character key to a character.
keyToChar (MkKey ks) =
    case (keySymToChar ks) of
        Just c  -> c
	Nothing -> error "keyToChar: Not a character key!"

isCharKey      (MkKey ks) = isJust (keySymToChar ks)
isBackSpaceKey (MkKey ks) = ks == vK_BACK
isTabKey       (MkKey ks) = ks == vK_TAB
--isLineFeedKey  (MkKey ks) = ks == vK_LINEFEED
isClearKey     (MkKey ks) = ks == vK_CLEAR
isReturnKey    (MkKey ks) = ks == vK_RETURN
isEscapeKey    (MkKey ks) = ks == vK_ESCAPE
isDeleteKey    (MkKey ks) = ks == vK_DELETE
--isMultiKeyKey  (MkKey ks) = ks == vK_MULTI_KEY
isHomeKey      (MkKey ks) = ks == vK_HOME
isLeftKey      (MkKey ks) = ks == vK_LEFT
isUpKey        (MkKey ks) = ks == vK_UP
isRightKey     (MkKey ks) = ks == vK_RIGHT
isDownKey      (MkKey ks) = ks == vK_DOWN
isPriorKey     (MkKey ks) = ks == vK_PRIOR
isPageUpKey    (MkKey ks) = ks == vK_PRIOR  -- same as isPriorKey
isNextKey      (MkKey ks) = ks == vK_NEXT
isPageDownKey  (MkKey ks) = ks == vK_NEXT   -- same as isNextKey
isEndKey       (MkKey ks) = ks == vK_END
--isBeginKey     (MkKey ks) = ks == vK_Begin
isShiftLKey    (MkKey ks) = ks == vK_SHIFT  -- can't distinguish left and right
isShiftRKey    (MkKey ks) = ks == vK_SHIFT
isControlLKey  (MkKey ks) = ks == vK_CONTROL -- ambidextrous
isControlRKey  (MkKey ks) = ks == vK_CONTROL
--isCapsLockKey  (MkKey ks) = ks == vK_Caps_Lock
--isShiftLockKey (MkKey ks) = ks == vK_Shift_Lock
--isMetaLKey     (MkKey ks) = ks == vK_Meta_L
--isMetaRKey     (MkKey ks) = ks == vK_Meta_R
--isAltLKey      (MkKey ks) = ks == vK_Alt_L
--isAltRKey      (MkKey ks) = ks == vK_Alt_R

----------------------------------------------------------------
-- End
----------------------------------------------------------------

