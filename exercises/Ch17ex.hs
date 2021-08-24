module Ch17ex where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Gen

-- List Applicative

data List a =
  Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil                   = Nil
  (<*>) Nil _                   = Nil
  (<*>) (Cons f fs) (Cons x xs) = Cons (f x) (fs <*> xs)

instance EqProp (List a) where
  (=-=) Nil Nil                 =  \x y -> True
  (=-=) _ Nil                   =  False
  (=-=) Nil _                   =  False
  (=-=) (Cons x xs) (Cons y ys) = ((x == y) && xs =-= ys)


-- Take from https://stackoverflow.com/questions/36055669/how-do-i-create-an-arbitrary-instance-for-a-recursive-datatype
instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [(1,nil), (4,cons)]
    where nil = return Nil
          cons = do
            h <- arbitrary
            t1 <- arbitrary
            return $ Cons h tl



main :: IO ()
main = do
  quickBatch $ (applicative Nil)
