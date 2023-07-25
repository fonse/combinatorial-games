module Util.List where
import Data.List ( inits, tails, find )
import Data.Maybe ( fromJust )

-- Like filter but the condition compares the element being analyzed to every other element in the list. If any satisfies, the element is removed.
filterIfAnotherElementSatisfies :: (a -> a -> Bool) -> [a] -> [a]
filterIfAnotherElementSatisfies = filterIfAnotherElementSatisfies' []

filterIfAnotherElementSatisfies' :: [a] -> (a -> a -> Bool) -> [a] -> [a]
filterIfAnotherElementSatisfies' _ _ [] = []
filterIfAnotherElementSatisfies' passed f (x:xs) =
  if any (f x) (passed ++ xs)
    then filterIfAnotherElementSatisfies' passed f xs
    else x : filterIfAnotherElementSatisfies' (x:passed) f xs

mex :: (Foldable t, Eq a, Num a, Enum a) => t a -> a
mex ns = fromJust $ find (`notElem` ns) [0..]