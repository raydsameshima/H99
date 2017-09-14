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

