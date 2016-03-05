Prob26.lhs

> module Prob26 where

Generate the combinations of K distinct objects chosen from the the N elements of a list.

> import Data.List (tails, subsequences)

List comprehensions:

> combinations :: Int -> [a] -> [[a]]
> combinations 0 _ = [[]]
> combinations n lst 
>   = [ y:ys | y:xs' <- tails lst
>            , ys <- combinations (n-1) xs']

do-notation:

> combinations' :: Int -> [a] -> [[a]]
> combinations' 0 _ = return []
> combinations' n lst = do
>   y:xs <- tails lst
>   ys <- combinations (n-1) xs
>   return (y:ys) 

Without using tails:

> combinations'' :: Int -> [a] -> [[a]]  
> combinations'' _ []     = [[]]
> combinations'' 0 _      = [[]]
> combinations'' n (x:xs) = (map (x:) lstWithout_x) ++ rest
>   where
>     lstWithout_x = combinations'' (n-1) xs
>     rest = combinations n xs 

> combinations''' :: Int -> [a] -> [[a]]
> combinations''' _ [] = [[]]
> combinations''' 0 _  = [[]]
> combinations''' n xs 
>   = [ (xs !! i) : x | i <- [0 .. (length xs)-1]
>                     , x <- combinations''' (n-1) (drop (i+1) xs)]

Using subsequences in Data.List, but this is super slow:

> combinations4 :: Int -> [a] -> [[a]]
> combinations4 k ns = filter ((k==) . length) (subsequences ns)

  *Prob26> combinations 3 [1..100]
  (9.99 secs, 1,730,114,288 bytes)
  *Prob26> combinations' 3 [1..100]
  (9.87 secs, 1,746,911,976 bytes)
  *Prob26> combinations'' 3 [1..100]
  (10.28 secs, 1,730,618,824 bytes)
  *Prob26> combinations''' 3 [1..100]
  (11.37 secs, 1,786,674,056 bytes)


