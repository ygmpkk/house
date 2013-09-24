module Graphics.UI.Japi.Constants where

import Graphics.UI.Japi.Types

{-  BOOLEAN   -}
j_TRUE                                 = JBool 1
j_FALSE                                = JBool 0

{-  ALIGNMENT   -}
j_LEFT                                 = 0
j_CENTER                               = 1
j_RIGHT                                = 2
j_TOP                                  = 3
j_BOTTOM                               = 4
j_TOPLEFT                              = 5
j_TOPRIGHT                             = 6
j_BOTTOMLEFT                           = 7
j_BOTTOMRIGHT                          = 8

{-  CURSOR   -}
j_DEFAULT_CURSOR                       = 0
j_CROSSHAIR_CURSOR                     = 1
j_TEXT_CURSOR                          = 2
j_WAIT_CURSOR                          = 3
j_SW_RESIZE_CURSOR                     = 4
j_SE_RESIZE_CURSOR                     = 5
j_NW_RESIZE_CURSOR                     = 6
j_NE_RESIZE_CURSOR                     = 7
j_N_RESIZE_CURSOR                      = 8
j_S_RESIZE_CURSOR                      = 9
j_W_RESIZE_CURSOR                      = 10
j_E_RESIZE_CURSOR                      = 11
j_HAND_CURSOR                          = 12
j_MOVE_CURSOR                          = 13

{-  ORIENTATION   -}
j_HORIZONTAL                           = Orientation 0
j_VERTICAL                             = Orientation 1

{-  FONTS   -}
j_PLAIN                                = 0
j_BOLD                                 = 1
j_ITALIC                               = 2
j_COURIER                              = 1
j_HELVETIA                             = 2
j_TIMES                                = 3
j_DIALOGIN                             = 4
j_DIALOGOUT                            = 5

{-  COLORS   -}
j_BLACK                                = Colour 0
j_WHITE                                = Colour 1
j_RED                                  = Colour 2
j_GREEN                                = Colour 3
j_BLUE                                 = Colour 4
j_CYAN                                 = Colour 5
j_MAGENTA                              = Colour 6
j_YELLOW                               = Colour 7
j_ORANGE                               = Colour 8
j_GREEN_YELLOW                         = Colour 9
j_GREEN_CYAN                           = Colour 10
j_BLUE_CYAN                            = Colour 11
j_BLUE_MAGENTA                         = Colour 12
j_RED_MAGENTA                          = Colour 13
j_DARK_GRAY                            = Colour 14
j_LIGHT_GRAY                           = Colour 15
j_GRAY                                 = Colour 16

{-  COMPONENTLISTENER   -}
j_NONE                                 = ComponentKind 0
j_LINEDOWN                             = ComponentKind 1
j_LINEUP                               = ComponentKind 2
j_AREADOWN                             = ComponentKind 3
j_AREAUP                               = ComponentKind 4

{-  MOUSELISTENER   -}
j_MOVED                                = EventKind 0
j_DRAGGED                              = EventKind 1
j_PRESSED                              = EventKind 2
j_RELEASED                             = EventKind 3
j_ENTERERD                             = EventKind 4
j_EXITED                               = EventKind 5
j_DOUBLECLICK                          = EventKind 6

{-  J_MOVED   -}
j_RESIZED                              = MovedKind 1
j_HIDDEN                               = MovedKind 2
j_SHOWN                                = MovedKind 3

{-  WINDOWLISTENER   -}
j_ACTIVATED                            = 0
j_DEACTIVATED                          = 1
j_OPENED                               = 2
j_CLOSED                               = 3
j_ICONIFIED                            = 4
j_DEICONIFIED                          = 5
j_CLOSING                              = 6

{-  IMAGEFILEFORMAT   -}
j_GIF                                  = 0
j_JPG                                  = 1
j_PPM                                  = 2
j_BMP                                  = 3
