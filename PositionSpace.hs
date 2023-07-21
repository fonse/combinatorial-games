module PositionSpace where
import Data.Bifunctor (second)

type Position a = (Bool, a)
type PositionSpace a = [Position a]

empty :: a -> PositionSpace a
empty x = [(False, x)]

extend :: (a -> [a]) -> (a -> a) -> PositionSpace a -> PositionSpace a
extend play ignore xs = xs >>= playOrIgnore
  where
    playOrIgnore (True, x) = [(True, ignore x)]
    playOrIgnore (False, x) = (False, ignore x) : ((True,) <$> play x)

calculate :: (a -> PositionSpace a) -> a -> [a]
calculate f x = snd <$> filter fst (f x)

class Invertible a where
  invert :: a -> a

mirror :: Invertible a => (a -> [a]) -> a -> [a]
mirror moves = (invert <$>) . moves . invert