Prob19.lhs

> module Prob19 where

> import Prob17 (split'')

Rotate a list N place to the left.
Hint: Use the predefined functions length and (++).

If n<0, convert the problem to the equivalent problem n>0 by adding the list's length to n.

> rotate
>   :: [a] -> Int -> [a]
> rotate xs n 
>   | n >= 0    = drop n xs ++ take n xs
>   | otherwise = reverse (rotate rList (-n))
>       where 
>         rList = reverse xs 


> -- rotate' xs n = take (length xs) $ drop (length xs + n) $ cycle xs

Another implementation using split'' in Prob17:

> rotate'' 
>   :: [a] -> Int -> [a]
> rotate'' lst n 
>   | n >= 0 = second ++ first
>   | otherwise = rotate'' lst (n + length lst)
>   where 
>     (first, second) = split'' lst n
