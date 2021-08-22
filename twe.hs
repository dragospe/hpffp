module TwiceWhenEven where

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do 
  x <- xs
  if even x
    then [x*x, x*x]
    else []


tWE :: [Integer] -> [Integer]
tWE xs = xs >>= (\x -> if even x
                          then [x * x, x * x]
                          else [])
