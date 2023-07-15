{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <" #-}
{-# HLINT ignore "Use >" #-}
module Game where

import Util ( filterIfAnotherElementSatisfies, mex )
import Data.List ( find )
import Data.Maybe ( fromJust, fromMaybe, isNothing )
import Data.Ratio ( denominator, numerator )

data Game = Game { left :: [Game], right :: [Game] }

----------------------------
---- Euqality and Order ----
----------------------------
-- G >= H iff no Gᴿ <= H and H <= no Gᴸ
g `gte` h = none (`lte` h) (right g) && none (g `lte`) (left h)
  where none f = not . any f

g `lte` h = h `gte` g

instance Eq Game where
  g == h = (g `gte` h) && (g `lte` h)

instance Ord Game where
  (<=) = lte
  (>=) = gte
  g > h = g >= h && g /= h
  g < h = g <= h && g /= h

-- Fuzzy operator
x ||| y = not (x <= y) && not (y >= x)
x |> y = x > y || x ||| y
x <| y = x < y || x ||| y

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
simplify' (Game ls rs) = deleteDominatedStrategies . bypassReversibeMoves $ Game (simplify <$> ls) (simplify <$> rs)

-- If any Left option M of G has itself a Right option Mᴿ <= G, then it will not affect the value of G if we replace M as a Left option of G by all the Left options of that Mᴿ
-- If any Right option M of G has itself a Left option Mᴸ >= G, then it will not affect the value of G if we replace M as a Right option of G by all the Right options of that Mᴸ
bypassReversibeMoves :: Game -> Game
bypassReversibeMoves g@(Game ls rs) = Game (bypassReversibeMoves' g ls reverseLeftMoveIfPossible) (bypassReversibeMoves' g rs reverseRightMoveIfPossible)

reverseLeftMoveIfPossible :: Game -> Game -> Maybe [Game]
reverseLeftMoveIfPossible m g = left <$> find (<= g) (right m)

reverseRightMoveIfPossible :: Game -> Game -> Maybe [Game]
reverseRightMoveIfPossible m g = right <$> find (>= g) (left m)

bypassReversibeMoves' :: Game -> [Game] -> (Game -> Game -> Maybe [Game]) -> [Game]
bypassReversibeMoves' g ms reverseFunction = ms >>= \m -> fromMaybe [m] (reverseFunction m g)

-- It won't affect the value of G if we delete dominated options but retain the options that dominated them
deleteDominatedStrategies :: Game -> Game
deleteDominatedStrategies (Game ls rs) = Game (removeSmaller ls) (removeLarger rs)

removeSmaller :: [Game] -> [Game]
removeSmaller = filterIfAnotherElementSatisfies (<=)

removeLarger :: [Game] -> [Game]
removeLarger = filterIfAnotherElementSatisfies (>=)

-------------------------------
---- Useful Games and Show ----
-------------------------------
zero = Game [] []
star = Game [zero] [zero]
up = Game [zero] [star]
down = Game [star] [zero]

nim :: Int -> Game
nim 0 = zero
nim n = Game xs xs
  where xs = nim <$> [0..(n-1)]

instance Show Game where
  show = show' . simplify

-- G needs to be in its simplifed form
show' g = fromJust $ head $ dropWhile isNothing candidates
  where
    candidates = [
      showRational <$> toMaybeRational g, -- Is it a number?
      showNimber <$> nimIndex g, -- Is it a nimber?
      showArrowNotation <$> arrowNotationIndex g, -- Is it n.↑?
      showArrowStarNotation <$> arrowStarNotationIndex g, -- Is it n.↑ + *?
      fmap (\n -> showRational n ++ "*") (toMaybeRational $ simplify $ g + star), -- Is it a number + *?
      Just (showGeneric g)]

showRational :: Rational -> String
showRational x
  | denominator x == 1 = show (numerator x)
  | otherwise = show (numerator x) ++ "/" ++ show (denominator x)

showNimber :: Int -> String
showNimber 0 = "0"
showNimber 1 = "*"
showNimber n = "*" ++ show n

showArrowNotation :: Int -> String
showArrowNotation (-2) = "⇓"
showArrowNotation (-1) = "↓"
showArrowNotation 0 = "0"
showArrowNotation 1 = "↑"
showArrowNotation 2 = "⇑"
showArrowNotation n
  | n > 0 = show n ++ ".↑"
  | otherwise = show n ++ ".↓"

showArrowStarNotation :: Int -> String
showArrowStarNotation (-2) = "⇓*"
showArrowStarNotation (-1) = "↓*"
showArrowStarNotation 0 = "*"
showArrowStarNotation 1 = "↑*"
showArrowStarNotation 2 = "⇑*"
showArrowStarNotation n
  | n > 0 = show n ++ ".↑+*"
  | otherwise = show n ++ ".↓+*"

showGeneric (Game ls rs) = "{ " ++ showMoves ls ++ " | " ++ showMoves rs ++ " }"
  where showMoves gs = unwords (show <$> gs)

toMaybeRational :: Game -> Maybe Rational
toMaybeRational (Game [] []) = Just 0
toMaybeRational (Game ls rs)
  | any isNothing (maybeLefts ++ maybeRights) = Nothing
  | any (\l -> any (l >=) rs) ls = Nothing
  | null maybeRights = (+1) <$> maxLeft
  | null maybeLefts = (\n -> n-1) <$> minRight
  | otherwise = maxLeft >>= \a -> minRight >>= \b -> return ((a + b) / 2)
  where
    maybeLefts = toMaybeRational <$> ls
    maybeRights = toMaybeRational <$> rs
    maxLeft = maximum <$> sequence maybeLefts
    minRight = minimum <$> sequence maybeRights

-- If G is *n, what is that n?
nimIndex :: Game -> Maybe Int
nimIndex (Game ls rs)
  | ls /= rs = Nothing
  | any isNothing ns = Nothing
  | otherwise = mex <$> sequence ns
  where ns = nimIndex <$> ls

-- If G is n.↑, what is that n?
arrowNotationIndex :: Game -> Maybe Int
arrowNotationIndex g
  | g == zero = Just 0
  | g < 0 = negate <$> arrowNotationIndex (-g)
arrowNotationIndex (Game [gl] [gr])
  | gl /= zero = Nothing
  | otherwise = arrowStarNotationIndex gr >>= \n -> if n >= 0 then Just (n+1) else Nothing
arrowNotationIndex _ = Nothing

-- If G is n.↑ + *, what is that n?
arrowStarNotationIndex :: Game -> Maybe Int
arrowStarNotationIndex g
  | g == up + star = Just 1
  | g < star = negate <$> arrowStarNotationIndex (-g)
arrowStarNotationIndex (Game [gl] [gr])
  | gl /= zero = Nothing
  | gr == zero = Just 0
  | otherwise = arrowNotationIndex gr >>= \n -> if n >= 0 then Just (n+1) else Nothing
arrowStarNotationIndex _ = Nothing

--------------------------
---- Num for Addition ----
--------------------------
instance Num Game where
  g + h = simplify $ Game ls rs
    where
      ls  = map (+h) (left g) ++ map (+g) (left h)
      rs = map (+h) (right g) ++ map (+g) (right h)

  negate (Game ls rs) = Game (negate <$> rs) (negate <$> ls)

  fromInteger n
    | n == 0 = zero
    | n > 0 = Game [fromInteger (n-1)] []
    | otherwise = Game [] [fromInteger (n+1)]

  -- These only make sense for games which are numbers, but are required by Num
  g * h = simplify $ Game (left1++left2) (right1++right2)
    where
      left1   = [ gl*h + g*hl - gl*hl | gl <- left g,  hl <- left h  ]
      left2   = [ gr*h + g*hr - gr*hr | gr <- right g, hr <- right h ]
      right1  = [ gl*h + g*hr - gl*hr | gl <- left g,  hr <- right h ]
      right2  = [ gr*h + g*hl - gr*hl | gr <- right g, hl <- left h  ]

  abs g = g * signum g
  signum g
    | g == zero = 0
    | g > zero  = 1
    | otherwise = -1

---------------------------------
---- Combinatorial Typeclass ----
---------------------------------
class Combinatorial a where
  leftMoves :: a -> [a]
  rightMoves :: a -> [a]
  toGame :: a -> Game
  toGame x = simplify $ Game (toGame <$> leftMoves x) (toGame <$> rightMoves x)