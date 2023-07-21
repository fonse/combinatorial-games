module Hackenbush where

import Game
import Util ( choose )
import Movespace ( applyMove, calculate, start, Movespace )

data Color = Blue | Red | Green deriving (Show, Eq)
data Branch = Branch { color :: Color, subtree :: [Branch] } deriving Show
type HackenbushTree = [Branch]

instance Combinatorial HackenbushTree where
  leftMoves = movesforColors [Blue, Green]
  rightMoves = movesforColors [Red, Green]

-- Positions after cutting exactly one branch of a color in the given list
movesforColors :: [Color] -> HackenbushTree -> [HackenbushTree]
movesforColors colors = calculate (movesforColors' colors)

movesforColors' :: [Color] -> HackenbushTree -> Movespace HackenbushTree
movesforColors' _ [] = start []
movesforColors' colors (b:bs) = applyMove (\tail -> (++tail) <$> rootCuts ++ subtreeCuts) (b:) (movesforColors' colors bs)
  where
    rootCuts = [[] | color b `elem` colors]
    subtreeCuts = (\t -> [Branch (color b) t]) <$> movesforColors colors (subtree b)