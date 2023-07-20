module ToadsAndFrogs where

import Game
import Movespace ( Movespace, start, applyMove, calculate )

data Batrachian = Toad | Frog | Blank deriving (Eq,Show)
type ToadsAndFrogs = [Batrachian]

toadMoves :: ToadsAndFrogs -> [ToadsAndFrogs]
toadMoves = calculate toadMoves'

toadMoves' :: ToadsAndFrogs -> Movespace ToadsAndFrogs
toadMoves' [] = start []
toadMoves' (Toad:Blank:xs) = applyMove (\tail -> Blank:Toad:tail) (\tail -> Toad:Blank:tail) (toadMoves' xs)
toadMoves' (Toad:Frog:Blank:xs) = applyMove (\tail -> Blank:Frog:Toad:tail) (\tail -> Toad:Frog:Blank:tail) (toadMoves' xs)
toadMoves' (x:xs) = (x:) <$> toadMoves' xs

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