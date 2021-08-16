{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

a = (* 9 ) 6 -- Num a => a
b = head [(0, "doge"), (1, "kitteh")] -- Num a => (a, [Char])
c = head [(0 :: Integer, "doge"), (1,"kitteh")] -- (Integer, [Char])
d = if False then True else False -- Bool
e = length [1, 2, 3, 4, 5] -- Int
f = (length [1, 2, 3, 4]) > (length "TACOCAT") -- Bool
