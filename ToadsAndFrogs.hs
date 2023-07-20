module ToadsAndFrogs where

import Game
import Data.Bifunctor (second)

data Batrachian = Toad | Frog | Blank deriving (Eq,Show)
type ToadsAndFrogs = [Batrachian]

toadMoves :: ToadsAndFrogs -> [ToadsAndFrogs]
toadMoves xs = snd <$> filter fst (toadMoves' xs)

toadMoves' :: ToadsAndFrogs -> [(Bool,ToadsAndFrogs)]
toadMoves' [] = [(False,[])]
toadMoves' (Toad:Blank:xs) = do
  (hasMoved,position) <- toadMoves' xs
  if hasMoved then return (True, Toad:Blank:position) else [(True, Blank:Toad:position), (False, Toad:Blank:position)]
toadMoves' (Toad:Frog:Blank:xs) = do
  (hasMoved,position) <- toadMoves' xs
  if hasMoved then return (True, Toad:Frog:Blank:position) else [(True, Blank:Frog:Toad:position), (False, Toad:Frog:Blank:position)]
toadMoves' (x:xs) = second (x:) <$> toadMoves' xs

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
