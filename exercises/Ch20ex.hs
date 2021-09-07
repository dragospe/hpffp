module Ch20ex where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
-- Exercises: Library Functions --
"Implement the functions in terms of `foldMap` or `foldr` from `Foldable`,
then try them out with multiple types that have Foldable Instances.

-}

-- Ex 1

sum :: (Foldable t, Num a) => t a -> a
sum xs = getSum $ foldMap Sum xs

-- Ex 2

product :: (Foldable t, Num a) => t a -> a
product xs = getProduct $ foldMap Product xs

-- Ex 3

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x xs = getAny $ foldMap (Any . (== x)) xs

-- Ex 4

{-

Below is my first attempt at the exercise, because forgot about the "Maybe"
instance of Ord.

Leaving it in for posterity, because it's useful for wrapping unbounded types
that still need \pm \infty values (with respect to ordering).

There an analogous Max' type, and I think there's a way to have an
"Unbounded" type that Min' and Max' could be newtypes of, but I'm not
going to worry so much about that right now.

------

Below is a solution from https://stackoverflow.com/questions/37398457/how-do-i-implement-minimum-with-foldr-or-foldmap

mini :: (Foldable f, Ord a) => f a -> Maybe a
mini = foldr maybeMin Nothing
  where
    maybeMin x Nothing = Just x
    maybeMin x (Just y) = Just (min x y)

-}

data Min' a = Min' {getMin' :: a} | Inf deriving (Eq, Show)

instance Ord a => Semigroup (Min' a) where
  (<>) (Min' x) (Min' y) = Min' (min x y)
  (<>) Inf Inf = Inf
  (<>) Inf (Min' y) = Min' y
  (<>) (Min' x) Inf = Min' x

instance Ord a => Monoid (Min' a) where
  mempty = Inf

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = gMin' $ foldMap Min' xs
  where
    gMin' :: Min' a -> Maybe a
    gMin' (Min' a) = Just a
    gMin' Inf = Nothing

instance Arbitrary a => Arbitrary (Min' a) where
  arbitrary = do
    a <- arbitrary
    oneof [return Inf, return a]

instance Eq a => EqProp (Min' a) where
  (=-=) = eq

testMin' :: IO ()
testMin' = do
  quickBatch $ semigroup (Min' (1 :: Int), 6 :: Int)
  quickBatch $ monoid (Min' (1 :: Int))

-- Ex 6
{- Cheated on this one, but... whoa. Wouldn't have thought of that easily. -}

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

-- Ex 7

length' :: Foldable t => t a -> Integer
length' xs = foldr f 0 xs
  where
    f :: (a -> Integer -> Integer)
    f _ n = n + 1

-- Ex 8

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- Ex 9

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- Ex 10

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr ((<>) . f) mempty xs

{----- Chapter Exercises -----}

-- Ex 1

data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f acc (Constant b) = f b acc

-- Ex 2

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f acc (Two _ b) = f b acc

-- Ex 4

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f acc (Three' _ b b') = (f b (f b' acc))

-- filterF exercise

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (f' f)
  where
    f' :: (Applicative f, Monoid (f a)) => (a -> Bool) -> a -> f a
    f' g x
      | g x = pure x
      | otherwise = mempty
