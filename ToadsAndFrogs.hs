module ToadsAndFrogs where

import Combinatorial ( PositionSpace, empty, extend, calculate, mirror, Invertible (..), Combinatorial (..) )

data Batrachian = Toad | Frog | Blank deriving (Eq,Show)
type ToadsAndFrogs = [Batrachian]

toadMoves :: ToadsAndFrogs -> PositionSpace ToadsAndFrogs
toadMoves [] = empty []
toadMoves (Toad:Blank:xs) = extend (\tail -> [Blank:Toad:tail]) (\tail -> Toad:Blank:tail) (toadMoves xs)
toadMoves (Toad:Frog:Blank:xs) = extend (\tail -> [Blank:Frog:Toad:tail]) (\tail -> Toad:Frog:Blank:tail) (toadMoves xs)
toadMoves (x:xs) = extend (const []) (x:) (toadMoves xs)

instance Invertible ToadsAndFrogs where
  invert = (reverseBatrachian <$>) . reverse
    where
      reverseBatrachian Toad = Frog
      reverseBatrachian Frog = Toad
      reverseBatrachian Blank = Blank

instance Combinatorial ToadsAndFrogs where
  leftMoves = calculate toadMoves
  rightMoves = mirror leftMoves