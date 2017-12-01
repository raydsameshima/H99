Prob26.lhs

> module Prob26 where

Generate the combinations of K distinct objects chosen from the N elements
of a list.

> import Data.List 
>   ( tails -- [1,2,3] -> [[1,2,3],[2,3],[3],[]]
>   , subsequences )

List comprehensions:

> combinations 
>   :: Int -> [a] -> [[a]]
> combinations 0 _ = [[]]
> combinations n lst 
>   = [ y:ys | y:xs <- tails lst
>            , ys <- combinations (n-1) xs ]
 
  *Prob26> combinations 2 [1..3]
  [[1,2],[1,3],[2,3]]

Let us trace this example step by step.
  c 2 [1,2,3]
    = [y:ys | y:xs <- [[1,2,3],[2,3],[3],[]], ys <- c 1 xs]

(y:xs)=[1,2,3] case, i.e., y=1 and 
  ys <- c 1 [2,3] 
        = [z:zs | z:ws <- [[2,3],[3],[]], zs <- [[]]]
        = [2:[], 3:[]]
        = [[2],[3]]
Therefore, we have [1,2] and [1,3] as elements.

(y:xs)=[2,3] case, i.e., y=2 and
  ys <- c 1 [3] = [[3]]
and [2,3] as the element.

(y:xs) = [3] case, i.e., y=3 and
  ys <- c 1 [] = []
but we can not pick any element from [].
So this case and the following (y:xs)=[] case do not contribute.
Thus c 2 [1,2,3] returns
  [[1,2],[1,3],[2,3]]

do-notation:

> combinations' 
>   :: Int -> [a] -> [[a]]
> combinations' 0 _   = return []
> combinations' n lst = do
>   y:xs <- tails lst
>   ys <- combinations (n-1) xs
>   return (y:ys) 

Without using tails:

> combinations'' 
>   :: Int -> [a] -> [[a]]
> combinations'' 0 _      = [[]]
> combinations'' _ []     = []
> combinations'' n (x:xs) = map (x:) (combinations'' (n-1) xs) 
>                          ++ (combinations'' n xs)

> {-
> combinations'' 
>   :: Int -> [a] -> [[a]]  
> combinations'' _ []     = [[]]
> combinations'' 0 _      = [[]]
> combinations'' n (x:xs) = (map (x:) lstWithout_x) ++ rest
>   where
>     lstWithout_x = combinations'' (n-1) xs
>     rest = combinations n xs 
> -}

> combinations''' 
>   :: Int -> [a] -> [[a]]
> combinations''' _ [] = [[]]
> combinations''' 0 _  = [[]]
> combinations''' n xs 
>   = [ (xs !! i) : x | i <- [0 .. (length xs)-1]
>                     , x <- combinations''' (n-1) (drop (i+1) xs)]

Using subsequences in Data.List, but this is super slow:

> combinations4 
>   :: Int -> [a] -> [[a]]
> combinations4 k ns = filter ((k==) . length) (subsequences ns)

  *Prob26> combinations 3 [1..100]
  (9.99 secs, 1,730,114,288 bytes)
  *Prob26> combinations' 3 [1..100]
  (9.87 secs, 1,746,911,976 bytes)
  *Prob26> combinations'' 3 [1..100]
  (10.28 secs, 1,730,618,824 bytes)
  *Prob26> combinations''' 3 [1..100]
  (11.37 secs, 1,786,674,056 bytes)



