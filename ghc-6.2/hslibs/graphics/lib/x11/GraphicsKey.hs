module GraphicsKey (
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
import X

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

newtype Key = MkKey KeySym deriving Show

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

-- Converts an X KeySym representing an ISO 8859-1 (Latin 1) character or one
-- of a few control characters to a Char.
-- Note! It is assumed that the KeySym encoding for Latin 1 characters agrees
-- with the Haskell character encoding!
keySymToChar :: KeySym -> Maybe Char
keySymToChar ks
    | xK_space <= ks && ks <= xK_ydiaeresis = Just (toEnum (fromIntegral ks))
    | ks == xK_BackSpace                    = Just '\BS'
    | ks == xK_Tab                          = Just '\HT'
    | ks == xK_Linefeed                     = Just '\LF'
    | ks == xK_Clear                        = Just '\FF'
    | ks == xK_Return                       = Just '\CR'
    | ks == xK_Escape                       = Just '\ESC'
    | ks == xK_Delete                       = Just '\DEL'
    | otherwise                             = Nothing    


-- Converts a character key to a character.
keyToChar (MkKey ks) =
    case (keySymToChar ks) of
        Just c  -> c
	Nothing -> error "keyToChar: Not a character key!"

isCharKey      (MkKey ks) = isJust (keySymToChar ks)
isBackSpaceKey (MkKey ks) = ks == xK_BackSpace
isTabKey       (MkKey ks) = ks == xK_Tab
--isLineFeedKey  (MkKey ks) = ks == xK_Linefeed
isClearKey     (MkKey ks) = ks == xK_Clear
isReturnKey    (MkKey ks) = ks == xK_Return
isEscapeKey    (MkKey ks) = ks == xK_Escape
isDeleteKey    (MkKey ks) = ks == xK_Delete
--isMultiKeyKey  (MkKey ks) = ks == xK_Multi_key
isHomeKey      (MkKey ks) = ks == xK_Home
isLeftKey      (MkKey ks) = ks == xK_Left
isUpKey        (MkKey ks) = ks == xK_Up
isRightKey     (MkKey ks) = ks == xK_Right
isDownKey      (MkKey ks) = ks == xK_Down
isPriorKey     (MkKey ks) = ks == xK_Prior
isPageUpKey    (MkKey ks) = ks == xK_Page_Up
isNextKey      (MkKey ks) = ks == xK_Next
isPageDownKey  (MkKey ks) = ks == xK_Page_Down
isEndKey       (MkKey ks) = ks == xK_End
--isBeginKey     (MkKey ks) = ks == xK_Begin
isShiftLKey    (MkKey ks) = ks == xK_Shift_L
isShiftRKey    (MkKey ks) = ks == xK_Shift_R
isControlLKey  (MkKey ks) = ks == xK_Control_L
isControlRKey  (MkKey ks) = ks == xK_Control_R
--isCapsLockKey  (MkKey ks) = ks == xK_Caps_Lock
--isShiftLockKey (MkKey ks) = ks == xK_Shift_Lock
--isMetaLKey     (MkKey ks) = ks == xK_Meta_L
--isMetaRKey     (MkKey ks) = ks == xK_Meta_R
--isAltLKey      (MkKey ks) = ks == xK_Alt_L
--isAltRKey      (MkKey ks) = ks == xK_Alt_R

----------------------------------------------------------------
-- End
----------------------------------------------------------------

