module TakeAndBreak where

import Game
import Util ( choose )
import Data.Bits ( Bits(testBit) )
import Data.List ( nub, sort )

data TakeAndBreak = TakeAndBreak { rules :: [Int], heaps :: [Int] } deriving Show

-- Can you remove n beans while leaving behind m heaps according to the rules?
validMove :: [Int] -> Int -> Int -> Bool
validMove rules beansRemoved heapsRemaining
  | beansRemoved <= length rules = (rules !! beansRemoved) `testBit` heapsRemaining
  | otherwise = False

-- Ways to break a heap using at most n splits
split :: Int -> Int -> [[Int]]
split n _
  | n < 0 = []
split 0 _ = [[]]
split heapSize 0 = [[heapSize]]
split heapSize 1 = map (\i -> filter (>0) [i, heapSize-i]) [0..heapSize`div`2]
split heapSize n = nub . (sort <$>) $ do
  heaps <- split heapSize (n-1)
  (h, hs) <- choose heaps
  (hs++) <$> split h 1

instance Combinatorial TakeAndBreak where
  leftMoves = rightMoves
  rightMoves (TakeAndBreak rules heaps) =
    do
      n <- [0..(length rules-1)]
      (h,hs) <- choose heaps
      let afterTake = h-n
      newHeaps <- split afterTake (afterTake-1)
      if validMove rules n (length newHeaps) then return (TakeAndBreak rules (newHeaps ++ hs)) else []