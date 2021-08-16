module Constant_Ch17 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a) 

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a 

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant f <*> Constant x = Constant (f <> x)

-- "Int String Int" type, selecting the "Sum" monoid for ints.
type ISI = Constant (Sum Int, String, Sum Int) (Sum Int, String, Sum Int)

main :: IO ()
main = do
  quickBatch (functor (undefined :: ISI))
  quickBatch (applicative (undefined :: ISI))
