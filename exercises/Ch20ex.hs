module Ch20ex where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  fold (Constant a b) = b  

instance Arbitrary b => Arbitrary (Constant a b) where
  abritrary = return arbitrary

instance EqProp (Constant a b) where
  (=-=) = eq


