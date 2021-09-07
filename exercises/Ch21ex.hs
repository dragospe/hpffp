module Ch21ex where

import Control.Applicative
import Data.Monoid (Sum)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Traversable Instances

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldr f z (Identity x) = f x z

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary x => Arbitrary (Identity x) where
  arbitrary = do Identity <$> arbitrary

instance (Eq x) => EqProp (Identity x) where
  (=-=) = eq

testIdentity :: IO ()
testIdentity =
  let trigger = undefined :: (Sum Int, Sum Int, Sum Int)
   in do
        quickBatch $ functor (Identity trigger)
        quickBatch $ foldable (Identity (undefined :: (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)))
        quickBatch $ traversable (Identity trigger)

-- Constant Instance

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show, Ord)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) (Constant x) (Constant y) = x `eq` y

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

{- Specialized type:
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr :: (a -> b -> b) -> b -> Constant x -> b
-}

instance Foldable (Constant a) where
  foldr _ z _ = z

{- Specialized type:
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse :: (a -> f b) -> (Constant x) -> f (constant
-}
instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

testConstant :: IO ()
testConstant = do
  quickBatch $ functor (Constant undefined :: Constant Int (Int, Int, Int))
  quickBatch $ foldable (Constant undefined :: Constant Int (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))
  quickBatch $ traversable (Constant undefined :: Constant Int (Sum Int, Sum Int, Sum Int))

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (4, return $ Yep a)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

testOptional :: IO ()
testOptional = do
  quickBatch $ functor (undefined :: Optional (Int, Int, Int))
  quickBatch $ foldable (undefined :: Optional (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))
  quickBatch $ traversable (undefined :: Optional (Sum Int, Sum Int, Sum Int))

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    frequency [(1, pure Nil), (5, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

{- Specialized type:
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse :: (a -> f b) -> List a -> f (List b)

Cons <$> f x ::

-}
instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

testList :: IO ()
testList = do
  quickBatch $ functor (undefined :: List (Int, Int, Int))
  quickBatch $ foldable (undefined :: List (Sum Int, Sum Int, Sum Integer, Sum Rational, [Int]))
  quickBatch $ traversable (undefined :: List (Sum Int, String, [Int]))

-- Three

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

testThree :: IO ()
testThree = do
  quickBatch $ functor (undefined :: Three String Char (Int, Int, Int))
  quickBatch $ foldable (undefined :: Three String Char (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))
  quickBatch $ traversable (undefined :: Three String Char (Sum Int, Sum Int, Sum Int))

-- Big

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return (Big a b1 b2)

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldr f z (Big _ b1 b2) = f b1 $ f b2 z

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

testBig :: IO ()
testBig = do
  quickBatch $ functor (undefined :: Big String (Int, Int, Int))
  quickBatch $ foldable (undefined :: Big String (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))
  quickBatch $ traversable (undefined :: Big String (Sum Int, Sum Int, Sum Int))

-- Skifree

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

{- Specialized type:
foldr :: Foldable t :: (a -> b -> b) -> b -> t a -> b

For S n a:
foldr :: (a -> b -> b) -> b -> S n a -> b

For n a:
foldr :: (a -> b -> b) -> b -> n a -> b

foldr f z na = b

-}
instance Foldable n => Foldable (S n) where
  foldr f z (S na a) = foldr f (f a z) na

{- Specialized type:
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

For S n a:
traverse :: Applicative f => (a -> f b) -> S n a -> f (S n b)

For n a:
traverse :: Applicative f => a -> f b -> n a -> f (n b)

S <$> traverse f na :: f (S nb)
-}

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

testS :: IO ()
testS = do
  quickBatch $ functor (undefined :: S Maybe (Int, Int, Int))
  quickBatch $ foldable (undefined :: S Maybe (String, Sum Int, Sum Int, Sum Int, Sum Int))
  quickBatch $ traversable (undefined :: S Maybe (Sum Int, Sum Int, Sum Int))

-- Tree

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    frequency
      [ (1, pure Empty),
        (5, Leaf <$> arbitrary),
        (5, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node t1 a t2) = foldr f (f a (foldr f z t2)) t1

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) =
    Node
      <$> traverse f t1
      <*> f a
      <*> traverse f t2

testTree :: IO ()
testTree = do
  quickBatch $ functor (undefined :: Tree (Int, Char, String))
  quickBatch $ foldable (undefined :: Tree (Sum Int, String, Sum Int, Integer, Sum Int))
  quickBatch $ traversable (undefined :: Tree (Sum Int, Char, String))
