module Movespace where
import Data.Bifunctor (second)

type Position a = (Bool, a)
newtype Movespace a = Movespace { positions :: [Position a]} deriving Show

instance Functor Movespace where
  fmap f (Movespace xs) = Movespace (second f <$> xs)

start :: a -> Movespace a
start x = Movespace [(False, x)]

applyMove :: (a -> b) -> (a -> b) -> Movespace a -> Movespace b
applyMove play ignore (Movespace xs) = Movespace $ xs >>= playOrIgnore
  where
    playOrIgnore (True, x) = [(True, ignore x)]
    playOrIgnore (False, x) = [(True, play x), (False, ignore x)]

calculate :: (a -> Movespace b) -> a -> [b]
calculate f x = snd <$> filter fst (positions (f x))