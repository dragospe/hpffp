module WordNumberTest where

import           Data.List           (sort)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen (oneof)
import           WordNumber          (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns [1] for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "return [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1,0,0]

  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

  describe "half" $ do
    it "2 x half is identity (Float)" $ do
      property $ \x -> prop_half (x :: Float)
    it "2 x half is identity (Double)" $ do
      property $ \x -> prop_half (x :: Double)
    it "2 x half is identity (Rational)" $ do
      property $ \x -> prop_half (x :: Rational)

  describe "sort" $ do
    it "sorted lists are sorted (Float)" $ do
      property $ \x -> prop_listOrdered (x :: [Float])

  describe "associativity of addition" $ do
    it "for rationals" $ do
      property $ \x -> fAssociative (+) (x :: Rational)

  describe "commutivity of addition" $ do
    it "for rationals" $ do
      property $ \x -> fCommutative (+) (x :: Rational)

  describe "associvity of *" $ do
    it "for rationals" $ do
      property $ \x -> fAssociative (*) (x :: Rational)

  describe "commutivity of *" $ do
    it "for rationals" $ do
      property $ \x -> fCommutative (*) (x :: Rational)

  describe "quotient and remainder" $ do
    it "For Integer: (quot x y) * y + (rem x y ) == x"$ do
      property $ \x y -> prop_quotRem x (y :: NonZero Integer)

  describe "division/modulo law" $ do
    it "For Integer: (div x y) * y + (mod x y) == x" $ do
      property $ \x y -> prop_divMod x (y :: NonZero Integer)


----------


half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_half :: (Eq a, Fractional a) => a -> Bool
prop_half x = halfIdentity x == x


-- Folds down a list and makes sure that each element is greater than the last
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)


prop_listOrdered  :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort



-- General associativity property
fAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
fAssociative f x y z = x `f` (y `f` z) == (x `f` y) `f` z

-- General Commutatative property
fCommutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
fCommutative f x y = x `f` y == y `f` x

prop_quotRem :: (Integral a) => a -> NonZero a -> Bool
prop_quotRem x (NonZero y) = (quot x y)*y + (rem x y) == x

prop_divMod :: (Integral a) => a -> NonZero a -> Bool
prop_divMod x (NonZero y) = (div x y) * y + (mod x y) == x
