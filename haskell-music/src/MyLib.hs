module MyLib where
import Codec.Midi


infixr 5:+:,:=:

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Eq, Show)
type Octave = Int
type Pitch = (PitchClass, Octave)
type Duration = Rational

data Primitive a = Note Duration a
            | Rest Duration
  deriving (Eq, Show)

data Music a =
  Prim (Primitive a)
  | Music a :+: Music a
  | Music a :=: Music a
  deriving (Eq, Show)

testMusic :: Music Pitch -- first two measures of twinkle twinkle
testMusic = Prim (Note (1/4) (C, 4)) 
            :+: Prim (Note (1/4) (C,4)) 
            :+: Prim (Note (1/4) (G,4)) 
            :+: Prim (Note (1/4) (G,4)) 
            :+: Prim (Note (1/4) (A,4)) 
            :+: Prim (Note (1/4) (A,4)) 
            :+: Prim (Note (1/2) (G,4)) 

