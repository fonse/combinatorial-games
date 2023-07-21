module ToadsAndFrogs where

import Game
import PositionSpace ( PositionSpace, empty, extend, calculate )

data Batrachian = Toad | Frog | Blank deriving (Eq,Show)
type ToadsAndFrogs = [Batrachian]

toadMoves :: ToadsAndFrogs -> [ToadsAndFrogs]
toadMoves = calculate toadMoves'

toadMoves' :: ToadsAndFrogs -> PositionSpace ToadsAndFrogs
toadMoves' [] = empty []
toadMoves' (Toad:Blank:xs) = extend (\tail -> [Blank:Toad:tail]) (\tail -> Toad:Blank:tail) (toadMoves' xs)
toadMoves' (Toad:Frog:Blank:xs) = extend (\tail -> [Blank:Frog:Toad:tail]) (\tail -> Toad:Frog:Blank:tail) (toadMoves' xs)
toadMoves' (x:xs) = extend (const []) (x:) (toadMoves' xs)

frogMoves :: ToadsAndFrogs -> [ToadsAndFrogs]
frogMoves xs = reverseGame <$> toadMoves (reverseGame xs)
  where
    reverseGame = (reverseBatrachian <$>) . reverse
    reverseBatrachian Toad = Frog
    reverseBatrachian Frog = Toad
    reverseBatrachian Blank = Blank

instance Combinatorial ToadsAndFrogs where
  leftMoves = toadMoves
  rightMoves = frogMoves