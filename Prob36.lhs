Prob36.lhs

> module Prob36 where

Determine the prime factors of a given positive integer.
Construct a list containing the prime factors and their multiplicity.

> import Prob35 (primeFactors, primeFactors')
> import Data.List (group)

  *Prob36> Data.List.group $ primeFactors 315
  [[7],[5],[3,3]]
  *Prob36> map length it
  [1,1,2]

  *Prob36> map head $ Data.List.group $ primeFactors 315
  [7,5,3]
  *Prob36> map length $ Data.List.group $ primeFactors 315
  [1,1,2]
  *Prob36> zip [7,5,3] it
  [(7,1),(5,1),(3,2)]

> primeFactorsMult :: Integral a => a -> [(a, Int)]
> primeFactorsMult n = zip factors' powers
>   where
>     factors  = primeFactors n
>     factors' = map head $ group $ factors 
>     powers  = map length $ group $ factors 

> primeFactorsMult' :: Integral a => a -> [(a, Int)]
> primeFactorsMult' n = zip factors' powers
>   where
>     factors  = primeFactors' n
>     factors' = map head $ group $ factors 
>     powers  = map length $ group $ factors 

  *Prob36> primeFactorsMult' 315
  [(3,2),(5,1),(7,1)]

