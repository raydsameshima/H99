Prob25.lhs

> module Prob25 where

Generate a random permutation of the elements of a list.

> import Data.List
>   ( nub 
>   , permutations
>   )
> import Prob23
>   ( infDices
>   , randomSelect )

Making an infinite list of indices, it takes first l elements (to terminate
this function!), and returns the given list with quasi-random indices.

> randomPermutation
>   :: [a] -> IO [a]
> randomPermutation ls = do
>   let l = length ls
>   is <- take l . nub <$> infDices l
>   return [ls !! i | i <- is]

Using Data.List.permutations, we can also make the following implementation.
Note that, this is [] safe, since permutations [] is a singleton [[]].

> randomPermutation'
>   :: [a] -> IO [a]
> randomPermutation' ls = head <$> randomSelect (permutations ls) 1
