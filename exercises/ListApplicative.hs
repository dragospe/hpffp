{- LANGUAGE FlexibleInstance -}

module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

---- List applicative exercises
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

listGen4 :: (Arbitrary a) => Gen (List a)
listGen4 = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Cons a (Cons b (Cons c (Cons d Nil))))

listGen4Int :: Gen (List Int)
listGen4Int = listGen4

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen4

instance Eq a => EqProp (List a) where (=-=) = eq

instance Semigroup (List a) where
  Nil <> y = y
  y <> Nil = y
  Cons x x' <> y = Cons x (x' <> y)

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f f' <*> l = (f <$> l) <> (f' <*> l)

-----
-- ZipList applicative

-- Helper functions
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n l = go n l Nil
  where
    go :: Int -> List a -> List a -> List a
    go 0 _ returnList = returnList
    go _ Nil returnList = returnList
    go n' (Cons x xs) returnList =
      go (n' - 1) xs (returnList `append` (Cons x Nil))

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs in take 3000 l
      ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

--- Wrong -- not totally sure what they're looking for here?
instance Applicative ZipList' where
  pure x = ZipList' [x]
  _ <*> ZipList' [] = ZipList' []
  ZipList' [] <*> _ = ZipList' []
  ZipList' l <*> ZipList' l' = ZipList' (go l l' [])
    where
      go :: [(a -> b)] -> [a] -> [b] -> [b]
      go [] _ rlist = rlist
      go _ [] rlist = rlist
      go (f : fs) (x : xs) rlist = (f x) : (go fs xs rlist)

--- Validation exercises

data Validation e a = F e | S a deriving (Eq, Show)

validationGen :: (Arbitrary e, Arbitrary a) => Gen (Validation e a)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  oneof $ [return a, return b]

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ F a, return $ S b]

instance Functor (Validation e) where
  fmap _ (F e) = F e
  fmap f (S a) = S (f a)

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) x y = x `eq` y

instance Monoid e => Applicative (Validation e) where
  pure x = S x
  S f <*> S x = S (f x)
  S _ <*> F e = F e
  F e <*> S _ = F e
  F e <*> F e' = F (e <> e')

--

data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Functor (Pair) where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative (Pair) where
  pure x = Pair x x
  (Pair x x') <*> (Pair y y') = Pair (x y) (x' y')

instance Eq a => EqProp (Pair a) where
  x =-= y = x `eq` y

main :: IO ()
main = do
  quickBatch (semigroup ((Cons (2 :: Int) Nil), (2 :: Int)))
  quickBatch (monoid (Cons (2 :: Int) Nil))
  quickBatch (functor (undefined :: List (String, String, Int)))
  quickBatch (applicative (undefined :: List (String, String, Int)))

  quickBatch (functor (undefined :: Validation String (String, String, Int)))
  quickBatch (applicative (undefined :: Validation String (String, String, Int)))

testPair :: IO ()
testPair = do
  quickBatch (functor (undefined :: Pair (String, Int, String)))
  quickBatch (applicative (undefined :: Pair (String, Int, String)))

--

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (Two a b) <*> (Two a' b') = Two (a <> a') (b b')

instance (Eq a, Eq b) => EqProp (Two a b) where
  t =-= t' = t `eq` t'

testTwo :: IO ()
testTwo = do
  quickBatch (functor (undefined :: Two [Int] (String, Int, String)))
  quickBatch (applicative (undefined :: Two [Int] (String, Int, String)))

--

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b c) <*> (Three a' b' c') = Three (a <> a') (b <> b') (c c')

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  t =-= t' = t `eq` t'

testThree :: IO ()
testThree = do
  quickBatch (functor (undefined :: Three [Int] [Char] (String, Int, String)))
  quickBatch (applicative (undefined :: Three [Int] [Char] (String, Int, String)))

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a b c) <*> (Three' a' b' c') = Three' (a <> a') (b b') (c c')

instance (Eq a, Eq b) => EqProp (Three' a b) where
  t =-= t' = t `eq` t'

testThree' :: IO ()
testThree' = do
  quickBatch (functor (undefined :: Three' [Int] (String, Int, String)))
  quickBatch (applicative (undefined :: Three' [Int] (String, Int, String)))

--
