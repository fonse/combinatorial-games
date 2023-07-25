module Games.TakeAndBreak where

import Data.Bits ( Bits(testBit) )
import Data.List ( nub, sort )
import Combinatorial ( PositionSpace, empty, extend, calculate, Combinatorial (..) )
import Control.Monad (guard)

type Heap = Int
type Rules = [Int]
data TakeAndBreak = TakeAndBreak { rules :: Rules, heaps :: [Heap] } deriving Show

-- Can you remove n beans while leaving behind m heaps according to the rules?
isValid :: Rules -> Int -> Int -> Bool
isValid rules beansRemoved heapsRemaining
  | beansRemoved < length rules = (rules !! beansRemoved) `testBit` heapsRemaining
  | otherwise = False

-- All the ways to break a heap into smaller heaps
split :: Heap -> [[Heap]]
split 0 = [[]]
split n = nub . (sort <$>) $ [n] : [ head:tail | head <- [1..n], tail <- split (n-head) ]

moves :: Rules -> [Heap] -> PositionSpace [Heap]
moves rules [] = empty []
moves rules (h:hs) = extend (\tail -> (++tail) <$> newHeaps) (h:) (moves rules hs)
  where
    newHeaps = do
      take <- [0..h]
      heaps <- split (h-take)
      guard $ isValid rules take (length heaps)
      return heaps

instance Combinatorial TakeAndBreak where
  leftMoves = rightMoves
  rightMoves (TakeAndBreak rules heaps) = TakeAndBreak rules <$> calculate (moves rules) heaps