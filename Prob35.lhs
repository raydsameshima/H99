Prob35.lhs

> module Prob35 where

Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

> import Prob31 (primes)

  *Prob35> takeWhile ((>) . floor . sqrt . fromIntegral $ 315) primes
  [2,3,5,7,11,13]

> divides :: Integral a => a -> a -> Bool
> divides n m = (n `mod` m == 0)
>
> primeFactors :: Integral a => a -> [a]
> primeFactors n = helper n primes []
>
> helper :: Integral a => a -> [a] -> [a] -> [a]
> helper n pList@(p:ps) factors
>   | n == 1        = factors
>   | n `divides` p = helper n' pList (p:factors)
>   | otherwise     = helper n  ps    factors
>   where n' = n `div` p

  *Prob35> primeFactors 315
  [7,5,3,3]

> primeFactors' :: Integral a => a -> [a]
> primeFactors' n = let
>   helper' n pList@(p:ps) factors
>     | n == 1        = factors
>     | n `divides` p = p : (helper' n' pList factors)
>     | otherwise     = helper' n ps factors
>     where n' = n `div` p
>   in
>     helper' n primes []

  *Prob35> primeFactors' 315
  [3,3,5,7]

