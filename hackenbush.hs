import Game

data Color = Blue | Red | Green deriving (Show, Eq)
data Branch = Branch { color :: Color, subtree :: [Branch] } deriving Show
type HackenbushTree = [Branch]

-- Return all possible lists obtained by removing exactly one element that matches a given predicate
conditionalRemovals :: (a -> Bool) -> [a] -> [[a]]
conditionalRemovals _ [] = []
conditionalRemovals f (a:as)
  | f a = as : otherRemovals
  | otherwise = otherRemovals
  where otherRemovals = (a:) <$> conditionalRemovals f as

-- Positions after cutting exactly one branch of a color in the given list
movesforColors :: [Color] -> HackenbushTree -> [HackenbushTree]
movesforColors colors bs = rootCuts ++ (bs >>= \b -> subtreeCuts b) 
  where
    rootCuts = conditionalRemovals (\b -> (color b) `elem` colors) bs
    subtreeCuts b = map (\t -> [Branch (color b) t]) (movesforColors colors (subtree b))

fromHackenbush :: HackenbushTree -> Game
fromHackenbush t = Game left right
  where
    left = fromHackenbush <$> movesforColors [Blue, Green] t
    right = fromHackenbush <$> movesforColors [Red, Green] t

-- Helpful for testing
readStack :: [Color] -> HackenbushTree
readStack [] = []
readStack (x:xs) = [Branch x (readStack xs)]

readColor :: Char -> Color
readColor 'b' = Blue
readColor 'r' = Red
readColor 'g' = Green
