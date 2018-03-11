Prob39.lhs

> module Prob39 where

A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

> import Prob31 (primes)

  *Prob39> dropWhile (<10) $ takeWhile (<20) primes
  [11,13,17,19]
  (0.01 secs, 3,617,392 bytes)

> primesR 
>   :: Integral a => 
>      a -> a -> [a]
> primesR lowerL upperL = dropWhile (< lowerL) $ takeWhile (< upperL) primes
