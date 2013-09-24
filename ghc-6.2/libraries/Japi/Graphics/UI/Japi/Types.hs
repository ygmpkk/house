module Graphics.UI.Japi.Types where

import Foreign
import Foreign.C

newtype Frame = Frame { unFrame :: CInt } -- deriving (Real, Enum, Ord, Num)

instance Show Frame where
    show f = show (unFrame f)
instance Eq Frame where
    a == b = (unFrame a) == (unFrame b)
instance Num Frame where
    a + b = Frame (unFrame a + unFrame b)
    fromInteger i = Frame (fromInteger i)

newtype Object = Object { unObject :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Object where
    show f = show (unObject f)
instance Eq Object where
    a == b = (unObject a) == (unObject b)
instance Num Object where
    a + b = Object (unObject a + unObject b)
    fromInteger i = Object (fromInteger i)

newtype Image = Image { unImage :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Image where
    show f = show (unImage f)
instance Eq Image where
    a == b = (unImage a) == (unImage b)
instance Num Image where
    a + b = Image (unImage a + unImage b)
    fromInteger i = Image (fromInteger i)

newtype Printer = Printer { unPrinter :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Printer where
    show f = show (unPrinter f)
instance Eq Printer where
    a == b = (unPrinter a) == (unPrinter b)
instance Num Printer where
    a + b = Printer (unPrinter a + unPrinter b)
    fromInteger i = Printer (fromInteger i)

newtype TextArea = TextArea { unTextArea :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show TextArea where
    show f = show (unTextArea f)
instance Eq TextArea where
    a == b = (unTextArea a) == (unTextArea b)
instance Num TextArea where
    a + b = TextArea (unTextArea a + unTextArea b)
    fromInteger i = TextArea (fromInteger i)

newtype TextField = TextField { unTextField :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show TextField where
    show f = show (unTextField f)
instance Eq TextField where
    a == b = (unTextField a) == (unTextField b)
instance Num TextField where
    a + b = TextField (unTextField a + unTextField b)
    fromInteger i = TextField (fromInteger i)

newtype JBool = JBool { unJBool :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show JBool where
    show f = show (unJBool f)
instance Eq JBool where
    a == b = (unJBool a) == (unJBool b)
instance Num JBool where
    a + b = JBool (unJBool a + unJBool b)
    fromInteger i = JBool (fromInteger i)

newtype DebugLevel = DebugLevel { unDebugLevel :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show DebugLevel where
    show f = show (unDebugLevel f)
instance Eq DebugLevel where
    a == b = (unDebugLevel a) == (unDebugLevel b)
instance Num DebugLevel where
    a + b = DebugLevel (unDebugLevel a + unDebugLevel b)
    fromInteger i = DebugLevel (fromInteger i)

newtype Pos   = Pos { unPos :: CInt } -- (Real, Enum, Ord, Num, Integral )

instance Show Pos where
    show f = show (unPos f)
instance Eq Pos where
    a == b = (unPos a) == (unPos b)
instance Num Pos where
    a + b = Pos (unPos a + unPos b)
    fromInteger i = Pos (fromInteger i)

newtype Canvas = Canvas { unCanvas :: CInt }  -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Canvas where
    show f = show (unCanvas f)
instance Eq Canvas where
    a == b = (unCanvas a) == (unCanvas b)
instance Num Canvas where
    a + b = Canvas (unCanvas a + unCanvas b)
    fromInteger i = Canvas (fromInteger i)

newtype EventKind = EventKind { unEventKind :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show EventKind where
    show f = show (unEventKind f)
instance Eq EventKind where
    a == b = (unEventKind a) == (unEventKind b)
instance Num EventKind where
    a + b = EventKind (unEventKind a + unEventKind b)
    fromInteger i = EventKind (fromInteger i)

newtype EventListener = EventListener { unEventListener :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show EventListener where
    show f = show (unEventListener f)
instance Eq EventListener where
    a == b = (unEventListener a) == (unEventListener b)
instance Num EventListener where
    a + b = EventListener (unEventListener a + unEventListener b)
    fromInteger i = EventListener (fromInteger i)

newtype ComponentKind = ComponentKind { unComponentKind :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show ComponentKind where
    show f = show (unComponentKind f)
instance Eq ComponentKind where
    a == b = (unComponentKind a) == (unComponentKind b)
instance Num ComponentKind where
    a + b = ComponentKind (unComponentKind a + unComponentKind b)
    fromInteger i = ComponentKind (fromInteger i)

newtype ComponentListener = ComponentListener { unComponentListener :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show ComponentListener where
    show f = show (unComponentListener f)
instance Eq ComponentListener where
    a == b = (unComponentListener a) == (unComponentListener b)
instance Num ComponentListener where
    a + b = ComponentListener (unComponentListener a + unComponentListener b)
    fromInteger i = ComponentListener (fromInteger i)

newtype MovedKind = MovedKind { unMovedKind :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show MovedKind where
    show f = show (unMovedKind f)
instance Eq MovedKind where
    a == b = (unMovedKind a) == (unMovedKind b)
instance Num MovedKind where
    a + b = MovedKind (unMovedKind a + unMovedKind b)
    fromInteger i = MovedKind (fromInteger i)

newtype Colour = Colour { unColour :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Colour where
    show f = show (unColour f)
instance Eq Colour where
    a == b = (unColour a) == (unColour b)
instance Num Colour where
    a + b = Colour (unColour a + unColour b)
    fromInteger i = Colour (fromInteger i)

newtype Orientation = Orientation { unOrientation :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Orientation where
    show f = show (unOrientation f)
instance Eq Orientation where
    a == b = (unOrientation a) == (unOrientation b)
instance Num Orientation where
    a + b = Orientation (unOrientation a + unOrientation b)
    fromInteger i = Orientation (fromInteger i)

newtype Panel = Panel { unPanel :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Panel where
    show f = show (unPanel f)
instance Eq Panel where
    a == b = (unPanel a) == (unPanel b)
instance Num Panel where
    a + b = Panel (unPanel a + unPanel b)
    fromInteger i = Panel (fromInteger i)

newtype Label = Label { unLabel :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Label where
    show f = show (unLabel f)
instance Eq Label where
    a == b = (unLabel a) == (unLabel b)
instance Num Label where
    a + b = Label (unLabel a + unLabel b)
    fromInteger i = Label (fromInteger i)

newtype Alert = Alert { unAlert :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Alert where
    show f = show (unAlert f)
instance Eq Alert where
    a == b = (unAlert a) == (unAlert b)
instance Num Alert where
    a + b = Alert (unAlert a + unAlert b)
    fromInteger i = Alert (fromInteger i)

newtype MenuBar = MenuBar { unMenuBar :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show MenuBar where
    show f = show (unMenuBar f)
instance Eq MenuBar where
    a == b = (unMenuBar a) == (unMenuBar b)
instance Num MenuBar where
    a + b = MenuBar (unMenuBar a + unMenuBar b)
    fromInteger i = MenuBar (fromInteger i)

newtype Menu = Menu { unMenu :: CInt }  -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show Menu where
    show f = show (unMenu f)
instance Eq Menu where
    a == b = (unMenu a) == (unMenu b)
instance Num Menu where
    a + b = Menu (unMenu a + unMenu b)
    fromInteger i = Menu (fromInteger i)

newtype MenuItem = MenuItem { unMenuItem :: CInt } -- (Show, Eq, Real, Enum, Ord, Num, Integral)

instance Show MenuItem where
    show f = show (unMenuItem f)
instance Eq MenuItem where
    a == b = (unMenuItem a) == (unMenuItem b)
instance Num MenuItem where
    a + b = MenuItem (unMenuItem a + unMenuItem b)
    fromInteger i = MenuItem (fromInteger i)


