module Main (main) where

import ObjectIO

main :: IO ()
main = startIO NDI () (openDialog undefined hello) []
    where hello	= Dialog "" (TextControl "Hello world!" []) [WindowClose (noLS closeProcess)]