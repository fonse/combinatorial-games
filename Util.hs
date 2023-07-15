module Util where
import Data.List ( inits, tails, find )
import Data.Maybe ( fromJust )

-- Return a list of all possible ways to choose an element of the list, zipped with the list of remaining elements
choose :: [a] -> [(a,[a])]
choose xs = zip xs $ zipWith (++) (inits xs) (tail (tails xs))

-- Like filter but the condition compares the element being analyzed to every other element in the list. If any satisfies, the element is removed.
filterIfAnotherElementSatisfies :: (a -> a -> Bool) -> [a] -> [a]
filterIfAnotherElementSatisfies = filterIfAnotherElementSatisfies' []

filterIfAnotherElementSatisfies' :: [a] -> (a -> a -> Bool) -> [a] -> [a]
filterIfAnotherElementSatisfies' _ _ [] = []
filterIfAnotherElementSatisfies' passed f (x:xs) =
  if any (f x) (passed ++ xs)
    then filterIfAnotherElementSatisfies' passed f xs
    else x : filterIfAnotherElementSatisfies' (x:passed) f xs

mex :: [Int] -> Int
mex ns = fromJust $ find (`notElem` ns) [0..]