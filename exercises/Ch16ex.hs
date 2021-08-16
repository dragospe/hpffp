{-# LANGUAGE FlexibleInstances #-}


module Ch16ex where

import           Test.QuickCheck
import           Test.QuickCheck.Function


-- Functor Laws

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

----------------------
-- Rearrange exercises

-- 1

data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap f (Second b) = Second b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return $ First x, return $ Second y]

-- 2

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c


--

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--------------------------
-- Write functor instances

-- 1

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return Finance, return $ Desk a, return $ Bloor b]

-- 2

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

-- 3

newtype Flip f a b =
  Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip (K b)

-- 4

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- 6

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- 7

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

-- 8

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- 9

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor (List) where
  fmap f Nil        = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

-- 10

data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- 11

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read f')   = Read (f . f')

check :: IO ()
check = do
  quickCheck (functorIdentity :: Sum Int Int -> Bool)
  quickCheck (functorCompose' :: Sum Char Int -> Fun Int Char -> Fun Char String -> Bool)

  quickCheck (functorIdentity :: Quant Int Int -> Bool)
  quickCheck (functorCompose' :: Quant Char Int -> Fun Int Char -> Fun Char String -> Bool)

  quickCheck (functorIdentity :: K Int Int -> Bool)
  quickCheck (functorCompose' :: K Char Int -> Fun Int Char -> Fun Char String -> Bool)

  quickCheck (functorIdentity :: Flip K Int Int -> Bool)
  quickCheck (functorCompose' :: Flip K Char Int -> Fun Int Char -> Fun Char String -> Bool)

