module Main where

import Codec.Midi


example :: Midi
example =
  Midi
    { fileType = SingleTrack
    , timeDiv  = TicksPerBeat 480
    , tracks   = [track]
    }

track :: Track Ticks
track =
  [ (0, NoteOn 0 60 100)
  , (480, NoteOff 0 60 0)
  , (0, NoteOn 0 64 100)
  , (480, NoteOff 0 64 0)
  ]

main :: IO ()
main = exportFile "test.mid" example
