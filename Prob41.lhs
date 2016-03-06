Prob41.lhs

> module Prob41 where

Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. 
Very rarely, the primes are both bigger than say 50. 
Try to find out how many such cases there are in the range 2..3000.

> import Prob40 (goldbach)

> goldbachList :: Integral a => a -> a -> [(a,(a,a))]
> goldbachList lowerL upperL
>   | lowerL > upperL = error "wrong input"
>   | otherwise = [(x, goldbach x)| x <- [lowerL .. upperL], even x]

> goldbachList' :: Integral t => t -> t -> t -> [(t, (t, t))]
> goldbachList' lowerL upperL threshold
>   = filter (\(_,(b,_)) -> b > threshold) $ goldbachList lowerL upperL

  *Prob41> goldbachList 9 20
  [(10,(3,7)),(12,(5,7)),(14,(3,11)),(16,(3,13)),(18,(5,13)),(20,(3,17))]
  (0.01 secs, 2,575,184 bytes)
  *Prob41> goldbachList' 3 2000 50
  [(992,(73,919)),(1382,(61,1321)),(1856,(67,1789)),(1928,(61,1867))]
  (2.27 secs, 793,645,824 bytes)
