module Let_to_where where

times_three = x * 5
   where x = 10 * 5 +y
         y = 10

waxOn = x*5
  where z = 7
        x = y ^ 2
        y = z + 8

triple x = x*3
