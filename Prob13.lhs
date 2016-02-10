Prob13.lhs

> module Prob13 where

Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly.
I.e. don't explicitly create tha sublists containing the duplicates, as in prob09.lhs, but only count them.
As in prob11.lhs, simplify the result list by replacing the singleton lists (1, X) by X.

> import Prob11 (ListItem(..))

> encode' :: Eq a => [a] -> [(Int,a)]
> encode' = foldr helper []
>     where
>       helper x [] = [(1,x)]
>       helper x (y@(a,b):ys)
>         | x == b    = (1+a,x) : ys
>         | otherwise = (1,x):y : ys

> encodeDirect :: Eq a => [a] -> [ListItem a]
> encodeDirect = map encodeHelper . encode'
>     where
>       encodeHelper (1,x) = Single x
>       encodeHelper (n,x) = Multiple n x

> encodeDirect' :: Eq a => [a] -> [ListItem a]
> encodeDirect' [] = []
> encodeDirect' (x:xs) 
>   | count == 1 = (Single x) : encodeDirect' xs
>   | otherwise  = (Multiple count x) : encodeDirect' rest 
>   where
>     count = 1 + (length matched)
>     (matched, rest) = span (== x) xs

> function :: Eq a => [a] -> ([a],[a])
> function = \x -> span (== head x) x

  *Prob13> function "haskell"
  ("h","askell")

