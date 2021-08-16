module Ch15ex where

import           Data.Monoid
import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Gen (oneof)


-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial


instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _                         = BoolConj False

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary (BoolConj) where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _                           = BoolDisj True

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary (BoolDisj) where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

-- 8

data Or a b =
  Fst a
  | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst x <> y = y
  Snd x <> _ = Snd x

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof  [return $ Fst a, return $ Snd b]

-- 9

newtype Combine a b = Combine { unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

{- I don't actually understand what needs to happen here for all the types to line up...


instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
   f <- arbitrary
   return $ Combine f

funcAssoc :: (Arbitrary a, Eq b, Semigroup b) => (a -> b) -> (a -> b) -> (a -> b) -> a -> Bool
funcAssoc f g h c = ((f <> (g <> h)) c) == (((f <> g) <> h) c)
-}


---------------
-- Monoid stuff
---------------

-- Monoid Properties

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity a = a <> mempty == a


-- Trivial

instance Monoid Trivial where
  mempty = Trivial

-- Identity

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

-- Two

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-- BoolConj

instance Monoid BoolConj where
  mempty = BoolConj True

-- Mempty
newtype Mem s a = Mem { runMem :: s -> (a, s) }


{- The first element of the tuple is just the first elements when
   either of the functions are applied to s, appended monoidally.
   The second chains the output of the right function to the output of the left
-}
instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem f' = Mem (\s -> (
                            fst (f s) <> fst (f' s),
                            snd $ f $  snd $ f' s))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))

f'= Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0


-- main

{-
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc (Sum Int))
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc (Sum Int) (Sum Rational))
  quickCheck (monoidLeftIdentity :: Two (Sum Int) (Sum Rational) -> Bool)
  quickCheck (monoidRightIdentity :: Two (String) ([Int]) -> Bool)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)


  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc [Int] [(String, Char)])

-}
