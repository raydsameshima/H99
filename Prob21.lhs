Prob21.lhs

> module Prob21 where

> import Prob17 (split'')

Insert an element at a given position into a list.

> insertAt 
>   :: a -> [a] -> Int -> [a]
> -- insertAt x [] n = [x]
> insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

  *Prob21> let f = \(a,b) x -> a ++ [x] ++ b
  *Prob21> f it 'X'
  "aiXueo"
  *Prob21> split'' "aiueo" 2
  ("ai","ueo")
  *Prob21> f it 'X'
  "aiXueo"

> insertAt' 
>   :: a -> [a] -> Int -> [a]
> insertAt' c lst n = helper slst c
>   where 
>     helper (a,b) c = a ++ [c] ++ b
>     slst = split'' lst (n-1) 

Using foldr with cons (:) which faster (O(n) v.s. O(n^2)).

> insertAt'' 
>   :: a -> [a] -> Int -> [a]
> insertAt'' y xs n = foldr helper [] xs'
>   where
>     xs' = zip xs [1..]
>     helper (x, i) xs
>       | i == n    = y:x:xs
>       | otherwise = x:xs

  *Prob21> insertAt 2 ones 100000
  (1.13 secs, 199,284,400 bytes)

  *Prob21> insertAt' 2 ones 100000
  (1.70 secs, 244,884,768 bytes)

  *Prob21> insertAt'' 2 ones 100000
  (1.35 secs, 219,284,040 bytes)
