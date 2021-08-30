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
{-
null :: (Foldable t) => t a -> Bool
null x = foldr (
-}
