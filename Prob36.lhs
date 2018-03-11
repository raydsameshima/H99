Prob36.lhs

> module Prob36 where

Determine the prime factors of a given positive integer.
Construct a list containing the prime factors and their multiplicity.

> import Data.Numbers.Primes (primeFactors)
> import Data.List (group)

> primeFactorsMult, primeFactorsMult'
>   :: Integral a =>
>        a -> [(a, Int)]
> primeFactorsMult n = zip factors' powers
>   where
>     factors  = primeFactors n
>     factors' = map head $ group $ factors 
>     powers   = map length $ group $ factors 

  *Prob36> primeFactorsMult 315
  [(3,2),(5,1),(7,1)]

> primeFactorsMult' = map encode . group . primeFactors
>   where
>     encode xs = (head xs, length xs)

So far they do perform equaly:

  *Prob36> primeFactorsMult 40239234273234231
  [(3,2),(4471026030359359,1)]
  (18.87 secs, 33,424,056,944 bytes)
  *Prob36> primeFactorsMult' 40239234273234231
  [(3,2),(4471026030359359,1)]
  (18.71 secs, 33,424,056,896 bytes)
