module MyLib (someFunc) where
import GHC.Exts.Heap (ClosureType(WHITEHOLE))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

infixr 5:+:,:=:

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Eq, Show)
type Octave = Int
type Pitch = (PitchClass, Octave)
type Duration = Rational

data Primitive a = Note Duration a
            | Rest Duration

data Music a =
  Prim (Primitive a)
  | Music a :+: Music a
  | Music a :=: Music a