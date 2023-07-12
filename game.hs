import Data.List

data Game = Game [Game] [Game]

leftMoves  (Game ls _) = ls
rightMoves (Game _ rs) = rs


----------------------------
---- Euqality and Order ----
----------------------------
-- G >= H iff no Gᴿ <= H and H <= no Gᴸ
g `gte` h = none (\gr -> gr `lte` h) (rightMoves g) && none (\hl -> g `lte` hl) (leftMoves h)
  where none f = not . (any f)
  
g `lte` h = h `gte` g

instance Eq Game where
  g == h = (g `gte` h) && (g `lte` h)

instance Ord Game where
  (<=) = lte
  (>=) = gte
  g > h = g >= h && g /= h
  g < h = g <= h && g /= h

-- Structural Equality
class StructuralEq a where  
  (===) :: a -> a -> Bool  

instance (StructuralEq a) => StructuralEq [a] where
  [] === [] = True
  (g:gs) === (h:hs) = g === h && gs === hs
  _ === _ = False

instance StructuralEq Game where
  (Game gl gr) === (Game hl hr) = gl === hl && gr === hr


---------------------------
---- Simplifying Games ----
---------------------------
simplify :: Game -> Game
simplify g
  | g === h = g
  | otherwise = simplify h
  where h = simplify' g

simplify' :: Game -> Game
simplify' = deleteDominatedStrategies . bypassReversibeMoves

-- If any Left option M of G has itself a Right option Mᴿ <= G, then it will not affect the value of G if we replace M as a Left option of G by all the Left options of that Mᴿ
-- If any Right option M of G has itself a Left option Mᴸ >= G, then it will not affect the value of G if we replace M as a Right option of G by all the Right options of that Mᴸ
bypassReversibeMoves :: Game -> Game
bypassReversibeMoves g@(Game ls rs) = Game (bypassReversibeMoves' g ls reverseLeftMoveIfPossible) (bypassReversibeMoves' g rs reverseRightMoveIfPossible)

reverseLeftMoveIfPossible :: Game -> Game -> Maybe [Game]
reverseLeftMoveIfPossible m g = 
  let maybeMr = find (\mr -> mr <= g) (rightMoves m)
  in fmap leftMoves maybeMr

reverseRightMoveIfPossible :: Game -> Game -> Maybe [Game]
reverseRightMoveIfPossible m g = 
  let maybeMl = find (\ml -> ml >= g) (leftMoves m)
  in fmap rightMoves maybeMl

bypassReversibeMoves' :: Game -> [Game] -> (Game -> Game -> Maybe [Game]) -> [Game]
bypassReversibeMoves' g ms reverseFunction = concat $ map (\m -> replacementFor m) ms
  where 
    replacementFor m = case (reverseFunction m g) of
      (Just gs) -> gs
      Nothing -> [m]
      
-- It won't affect the value of G if we delete dominated options but retain the options that dominated them
deleteDominatedStrategies :: Game -> Game
deleteDominatedStrategies (Game ls rs) = Game (removeSmaller ls) (removeLarger rs)

-- Like filter but the condition compares every other element in the list to the element being analyzed. If any satisfies, the element is removed.
filterIfAnotherElementSatisfies :: (a -> a -> Bool) -> [a] -> [a]
filterIfAnotherElementSatisfies = filterIfAnotherElementSatisfies' []

removeSmaller :: [Game] -> [Game]
removeSmaller = filterIfAnotherElementSatisfies (>=)

removeLarger :: [Game] -> [Game]
removeLarger = filterIfAnotherElementSatisfies (<=)


-------------------------------
---- Useful Games and Show ----
-------------------------------
zero = Game [] []
star = Game [zero] [zero]
up = Game [zero] [star]
down = Game [star] [zero]
doubleup = up + up

instance Show Game where
  show g
    | g == zero = "0"
    | g == star = "*"
    | g == up = "↑"
    | g == down = "↓"
    | g == doubleup = "⇑"
    | otherwise = show' $ simplify g

show' (Game ls rs) = "{ " ++ (showMoves ls) ++ " | " ++ (showMoves rs) ++ " }"
  where showMoves gs = intercalate " " (map show gs)


--------------------------
---- Num for Addition ----
--------------------------
instance Num Game where
  g + h = Game left right
    where
      left  = map (+h) (leftMoves g) ++ map (+g) (leftMoves h)
      right = map (+h) (rightMoves g) ++ map (+g) (rightMoves h)

  negate (Game ls rs) = Game (map negate rs) (map negate ls)

  fromInteger n
    | n == 0 = zero
    | n > 0 = Game [fromInteger (n-1)] []
    | otherwise = Game [] [fromInteger (n+1)]

  -- These only make sense for games which are numbers, but are required by Num
  g * h = Game (left1++left2) (right1++right2)
    where
      left1   = [ gl*h + g*hl - gl*hl | gl <- leftMoves g,  hl <- leftMoves h  ]
      left2   = [ gr*h + g*hr - gr*hr | gr <- rightMoves g, hr <- rightMoves h ]
      right1  = [ gl*h + g*hr - gl*hr | gl <- leftMoves g,  hr <- rightMoves h ]
      right2  = [ gr*h + g*hl - gr*hl | gr <- rightMoves g, hl <- leftMoves h  ]

  abs g = g * signum g
  signum g
    | g == zero = zero
    | g > zero  = fromInteger 1
    | otherwise = fromInteger (-1)


-----------------------
---- Aux functions ----
-----------------------
filterIfAnotherElementSatisfies' :: [a] -> (a -> a -> Bool) -> [a] -> [a]
filterIfAnotherElementSatisfies' _ _ [] = []
filterIfAnotherElementSatisfies' passed f (x:xs) = 
  if any (\y -> f y x) (passed ++ xs)
    then filterIfAnotherElementSatisfies' passed f xs
    else x : filterIfAnotherElementSatisfies' (x:passed) f xs