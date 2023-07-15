import Game
import Util ( choose )

data Color = Blue | Red | Green deriving (Show, Eq)
data Branch = Branch { color :: Color, subtree :: [Branch] } deriving Show
type HackenbushTree = [Branch]

instance Combinatorial HackenbushTree where
  leftMoves = movesforColors [Blue, Green]
  rightMoves = movesforColors [Red, Green]

-- Positions after cutting exactly one branch of a color in the given list
movesforColors :: [Color] -> HackenbushTree -> [HackenbushTree]
movesforColors colors bs = rootCuts ++ (choose bs >>= subtreeCuts)
  where
    rootCuts = snd <$> filter (\(b,_) -> color b `elem` colors) (choose bs)
    subtreeCuts (b,bs) = map (\t -> Branch (color b) t : bs) (movesforColors colors (subtree b))