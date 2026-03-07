module MyLib where
import Codec.Midi
import GHC.TypeError (Assert)

infixr 5:+:,:=:

data PitchClass = Cdf | Cf | C | Cs | Cds 
                  | Ddf | Df | D | Ds | Dds
                  | Edf | Ef | E | Es | Eds 
                  | Fdf | Ff | F | Fs | Fds
                  | Gdf | Gf | G | Gs | Gds 
                  | Adf | Af | A | As | Ads
                  | Bdf | Bf | B | Bs | Bds
  deriving (Eq, Show)
type Octave = Int
type Pitch = (PitchClass, Octave)
type Duration = Rational
type AbsPitch = Int

data Primitive a = Note Duration a
            | Rest Duration
  deriving (Eq, Show)

a440 :: Pitch
a440 = (A, 4)

data Music a =
  Prim (Primitive a)
  | Music a :+: Music a
  | Music a :=: Music a
  deriving (Eq, Show)

qn :: Duration 
qn = 1/4

a440m :: Music Pitch
a440m = Prim (Note qn a440)

testMusic :: Music Pitch -- first two measures of twinkle twinkle
testMusic = Prim (Note (1/4) (C, 4)) 
            :+: Prim (Note (1/4) (C,4)) 
            :+: Prim (Note (1/4) (G,4)) 
            :+: Prim (Note (1/4) (G,4)) 
            :+: Prim (Note (1/4) (A,4)) 
            :+: Prim (Note (1/4) (A,4)) 
            :+: Prim (Note (1/2) (G,4)) 


pcToInt :: PitchClass -> Int
pcToInt pc = case pc of
  Cdf -> -2; Cf -> -1; C -> 0; Cs -> 1; Cds -> 2;
  Ddf -> 0; Df -> 1; D -> 2; Ds -> 3; Dds -> 4;
  Edf -> 2; Ef -> 3; E -> 4; Es -> 5; Eds -> 6;
  Fdf -> 3; Ff -> 4; F -> 5; Fs -> 6; Fds -> 7;
  Gdf -> 5; Gf -> 6; G -> 7; Gs -> 8; Gds -> 9;
  Adf -> 7; Af -> 8; A -> 9; As -> 10; Ads -> 11;
  Bdf -> 9; Bf -> 10; B -> 11; Bs -> 12; Bds -> 13

pitch :: AbsPitch -> Pitch
pitch ap =
  let (oct, n) = divMod ap 12
  in ([C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] !!n, oct)

absPitch :: Pitch -> AbsPitch
absPitch (pc, oct) = 12 * oct + pcToInt pc

trans :: Int -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)

note :: Duration -> a -> Music a
note d p = Prim (Note d p)

rest :: Duration -> Music a
rest d = Prim (Rest d)

mapPrim :: (Primitive a -> Primitive b) -> Music a -> Music b
mapPrim = undefined

transposeP :: AbsPitch -> Primitive Pitch -> Primitive Pitch
transposeP = undefined

transposeM :: AbsPitch -> Music Pitch -> Music Pitch
transposeM  = mapPrim . transposeP 

musicToMidi :: Music Pitch -> Midi
musicToMidi = undefined

wholeToneScale :: Pitch -> [Music Pitch]
wholeToneScale p = let f ap = note qn (pitch (absPitch p + ap))
                    in map f [0,2,4,6,8]