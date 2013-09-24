module GraphicsEvent
	( Event(..)
	) where

import GraphicsTypes( Point )
import GraphicsKey (Key)

-- We probably need a lot more info about the event 
-- but this will do for now.

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- Note: The Char event is for delivering properly translated characters
-- after a key*press*. At least under X, a single key press might yield
-- 0, 1 or more characters after translation (see X[mb]LookupString).
-- The Key event is intended for reporting key up/down events of
-- *abstract* keys, i.e. KeySyms rather than KeyCodes in X terms.
-- To make it possible to report such events for arrow keys, function
-- keys and the like, the Char field needs to be replaced by a field of
-- a type somewhat isomorphic to KeySym, but valid under Windows too.

data Event 
  = Char      { char :: Char }
  | Key       { keysym :: Key, isDown :: Bool }
  | Button    { pt :: Point, isLeft, isDown :: Bool }
  | MouseMove { pt :: Point }
  | Resize
  | Closed
 deriving Show 

----------------------------------------------------------------
-- End
----------------------------------------------------------------
