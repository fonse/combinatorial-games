module Thermograph where

import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

-- A thermograph consists of a left Boundary (where segments decrease in value) and a right Boundary (where segments increase in value)
-- Boundaries always begin with a segment with temperature 0 and alternate between Vertical and Diagonal segments, but the last segment is always a Vert
-- The EndlessDiag segment type is used only in internal computation as the result of taxing the final Vert
type Value = Rational
data Segment = Vert { value :: Value, temp :: Rational } | Diag { start :: Value, end :: Value, temp :: Rational } | EndlessDiag {start :: Value, temp :: Rational} deriving (Eq, Show)
type Boundary = [Segment]
data Thermograph = Thermograph { leftBoundary :: Boundary, rightBoundary :: Boundary} deriving (Eq, Show)

----------------------------
---- Exported Functions ----
----------------------------
-- Create a thermograph for a cold game
coldThermograph :: Value -> Thermograph
coldThermograph v = Thermograph [Vert v 0] [Vert v 0]

-- Calculate the thermograph of a game given the thermographs of its positions
calculateThermograph :: [Thermograph] -> [Thermograph] -> Thermograph
calculateThermograph ls rs = Thermograph (truncateBoundaryL meanValue taxedRightBoundary) (truncateBoundaryR meanValue taxedLeftBoundary)
  where
    taxedRightBoundary = taxL . combineL $ rightBoundary <$> ls
    taxedLeftBoundary = taxR . combineR $ leftBoundary <$> rs
    meanValue = findIntersection taxedRightBoundary taxedLeftBoundary

meanValueT :: Thermograph -> Rational
meanValueT = lastValue . leftBoundary
  where
    lastValue [x] = value x
    lastValue (x:xs) = lastValue xs

freezingPointT :: Thermograph -> Rational
freezingPointT = lastTemperature . leftBoundary
  where
    lastTemperature [x] = temp x
    lastTemperature (x:xs) = lastTemperature xs

---------------------------------
---- Thermograph Calculation ----
---------------------------------
-- Combine multiple right Boundaries into a single one (Being right boundaries the goal is to maximize the value)
-- For this calculation it's useful to traverse the Boundary "backwards", starting at the final Vert and ending at temp=0
combineL :: [Boundary] -> Boundary
combineL ls = reverseBoundary . combineL' . sortBoundaries $ (reverseBoundary <$> ls)

combineL' :: [Boundary] -> Boundary
combineL' [] = []
combineL' [l] = l
combineL' (l:l2:ls) = case span (diagEntirelyGreaterThan boundary) l of
  (head, []) -> head
  (head, (Diag start end temp):tail) -> head ++ Diag start boundary (temp + boundary - end) : combineL' (sortBoundaries (prependIfNotEmpty tail (l2:ls)))
  where
    boundary = startingValue l2
    prependIfNotEmpty [] xs = xs
    prependIfNotEmpty x xs = x:xs

combineR :: [Boundary] -> Boundary
combineR ls = mirrorBoundary . combineL $ mirrorBoundary <$> ls

-- Tax a right boundary by turning diagonals into vertical lines and vice versa, shifting everything to the right (smaller values)
taxL :: Boundary -> Boundary
taxL = taxL' 0

taxL' :: Rational -> [Segment] -> [Segment]
taxL' _ [] = []
taxL' tax [Vert v t] = [EndlessDiag (v - tax) t]
taxL' tax (Vert v t : x : xs) = Diag v (v - newTax) t : taxL' newTax (x:xs)
  where newTax = tax + temp x - t
taxL' tax (Diag start end t : xs) = Vert (start - tax) t : taxL' newTax xs
  where newTax = tax + end - start

taxR :: Boundary -> Boundary
taxR = mirrorBoundary . taxL . mirrorBoundary

-- Calculate the intersection of two taxed boundaries
findIntersection :: Boundary -> Boundary -> Value
findIntersection (r:rs) (l:ls) = case findIntersectionSegment r l minSecondTemp of
  Just value -> value
  Nothing
    | secondTempR > secondTempL  -> findIntersection (setTemp minSecondTemp (-1) r : rs) ls
    | secondTempR < secondTempL  -> findIntersection rs (setTemp minSecondTemp 1 l : ls)
    | bothEndlessDiags           -> findIntersection [setTemp minSecondTemp (-1) r] [setTemp minSecondTemp 1 l]
    | otherwise                  -> findIntersection rs ls
  where
    secondTempR = secondTemp (r:rs)
    secondTempL = secondTemp (l:ls)
    minSecondTemp = min secondTempR secondTempL
    bothEndlessDiags = null rs && null ls

-- Find the intersection between segments that start at the same temperature
findIntersectionSegment :: Segment -> Segment -> Rational -> Maybe Value
findIntersectionSegment (Diag startR _ tempR) l maxTemp = findIntersectionSegment (EndlessDiag startR tempR) l maxTemp
findIntersectionSegment r (Diag startR _ tempR) maxTemp = findIntersectionSegment r (EndlessDiag startR tempR) maxTemp

findIntersectionSegment (EndlessDiag startR temp) (EndlessDiag startL _) maxTemp
  | startR - maxTemp + temp > startL + maxTemp - temp = Nothing
  | otherwise = Just ((startR + startL) / 2)

findIntersectionSegment (EndlessDiag startR temp) (Vert startL _) maxTemp
  | startR - maxTemp + temp > startL = Nothing
  | otherwise = Just startL

findIntersectionSegment (Vert startR _) (EndlessDiag startL temp) maxTemp
  | startR > startL + maxTemp - temp = Nothing
  | otherwise = Just startR

findIntersectionSegment (Vert startR _) (Vert startL _) maxTemp
  | startR == startL = Just startR
  | otherwise = Nothing

-- Truncate a boundary at a given value, making sure it ends with a Vert
truncateBoundaryL :: Value -> Boundary -> Boundary
truncateBoundaryL = truncateBoundary' GT (-1)

truncateBoundaryR :: Value -> Boundary -> Boundary
truncateBoundaryR = truncateBoundary' LT 1

truncateBoundary' :: Ordering -> Rational -> Value -> Boundary -> Boundary
truncateBoundary' ord direction v (segment@Vert{} : xs)
  | value segment == v = [segment]
  | otherwise  = segment : truncateBoundary' ord direction v xs

truncateBoundary' ord direction v (segment@(Diag start end temp) : xs)
  | compare end v /= ord = [Diag start v temp, Vert v (temp + (v - start) * direction)]
  | otherwise  = segment : truncateBoundary' ord direction v xs

truncateBoundary' _ direction v [EndlessDiag start temp] = [Diag start v temp, Vert v (temp + (v - start) * direction)]

--------------------------
---- Helper Functions ----
--------------------------
-- Mirror a Boundary so we can use Left's function on Right
mirrorBoundary :: Boundary -> Boundary
mirrorBoundary xs = mirrorSegment <$> xs
  where
    mirrorSegment (Vert v t) = Vert (-v) t
    mirrorSegment (Diag v1 v2 t) = Diag (-v1) (-v2) t
    mirrorSegment (EndlessDiag v t) = EndlessDiag (-v) t

-- Reverse the direction of a Boundary
reverseBoundary :: Boundary -> Boundary
reverseBoundary xs = flipSegment <$> reverse xs
  where
    flipSegment (Diag v1 v2 t) = Diag v2 v1 t
    flipSegment x = x

-- Get the value of the initial Vert of a reversed Boundary
startingValue :: [Segment] -> Value
startingValue = value . head

-- Sort a list of reversed Boundaries by their starting value
sortBoundaries :: [Boundary] -> [Boundary]
sortBoundaries = sortBy (comparing (Down . startingValue))

-- Used with `span` to split a Boundary into two sections, left and right of a given value
diagEntirelyGreaterThan :: Value -> Segment -> Bool
diagEntirelyGreaterThan v (Diag _ end _) = end > v
diagEntirelyGreaterThan _ _ = True

-- Get the second temperature breakpoint of a Boundary
secondTemp :: Boundary -> Rational
secondTemp (Vert _ _:x:xs) = temp x
secondTemp (Diag start end temp:_) = temp + start - end
secondTemp (EndlessDiag _ temp:_) = temp + 10 -- This is good enough for the caller of this function

-- Replace the temperature of a Segment with a higher one. If it's a Diag the new value should still be low enough to be consistent.
setTemp :: Rational -> Rational -> Segment -> Segment
setTemp t _ (Vert value _) = Vert value t
setTemp t direction (Diag start end temp) = Diag (start + (t - temp) * direction) end t
setTemp t direction (EndlessDiag start temp) = EndlessDiag (start + (t - temp) * direction) t