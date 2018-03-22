Prob15.lhs

> module Prob15 where 
> import Data.List (replicate, transpose)

Replicate the elements of a list a given number of times.

> repli 
>   :: [a] -> Int -> [a]
> repli []     _ = []
> repli [x]    n = replicate n x
> repli (x:xs) n = (repli [x] n) ++ (repli xs n) 

  repli xs n = concatMap (replicate n) xs

or, using the list monad:

  repli xs n = xs >>= replicate n

Another implementation using list comprehension which is similar to Prob14:

> repli' 
>   :: [a] -> Int -> [a]
> repli' lst n = [x | x <- lst, _ <- [1..n]]

Or even using replicate,

> repli'' xs n = concat . transpose . replicate n $ xs
> repli''' xs n = xs >>= replicate n
