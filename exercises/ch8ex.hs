module Ch8ex where

-- Recursive implementation of summing the first N natural numbers
sumToN :: Integral a => a -> a
sumToN n = go n 0 0
  where go m s count -- s is our running "sum"
         | count == m = s + count
         | otherwise = go m (s + count) (count + 1)

-- Recursive implementation of multiplication
mult :: Integral a => a -> a -> a
mult x y = go x y 0 0
  where  go i j prod count
          | count == j = prod 
          | otherwise = go i j (prod + i) (count + 1)


---- Improvement on chapter 8's dividedBy function
--- NOTE: This doesn't totally work. I wanted to make it a tuple like divMod 
--- (even though the exercise didn't ask for it) but didn't want to fuck around
--- with the different cases for the sign and modulo of the remainder. Mostly it works, 
--- but the remainder term is only accurate module +- denominator.
--
-- We add a new datatype to account for the "division by zero case"
data DividedByResult = Result (Integer, Integer) | DividedByZero deriving Show
--
-- Change the type of the function to reflect this
dividedBy :: Integer -> Integer -> DividedByResult
--
-- And introduce a helper function "absDividedBy" that takes the absolute
-- value of the arguments before dividing.
dividedBy num denom 
  -- Division by zero case:
  | denom == 0 = DividedByZero
  -- 0 numerator case
  | num == 0 = Result (0,0)
  -- Signs match case
  | signum num == signum denom = Result (absDividedBy num denom) --  result.
  -- Opposite signs cases
  | num > 0 = Result ((\(x,y) -> ((-1) * x - 1, (-1) * y `mod` (-1) * denom)) (absDividedBy num denom))
  | otherwise = Result ((\(x,y) -> ((-1) * x - 1, y)) (absDividedBy num denom)) 
  -- And the "dividedBy" function from the book, but restricted to positive integers only
  where absDividedBy :: Integer -> Integer -> (Integer, Integer)
        absDividedBy n d = go (abs n) (abs d) 0
            where go n' d' count
                   | n' < d' = (count, n')
                   | otherwise = go (n' - d') d' (count + 1)



-- McCarthy 91 function. Returns  x - 10 when x> 100 and 91 otherwise. It is recursive.

mc91 :: (Num a, Ord a) => a -> a
mc91 n 
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11

