module Main where

import Euterpea

scale :: Music Pitch
scale = line [c 4 qn, d 4 qn, e 4 qn, f 4 qn, g 4 qn]

main :: IO ()
main = writeMidi "scale.mid" scale