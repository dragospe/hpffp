module Test where

data Nat = Zero | Succ Nat deriving (Eq, Show)

integerToNat :: Integer -> Maybe Nat
integerToNat i = go i Zero
  where go :: Integer -> Nat -> Maybe Nat
        go i n
          | i < 0 = Nothing
          | i == 0 = Just n
          | i > 0 = go (i - 1) (Succ n)
          | otherwise = error "This shouldn't be possible"
