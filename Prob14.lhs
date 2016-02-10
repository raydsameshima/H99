Prob14.lhs

> module Prob14 where

Duplicate the elements of a list.

> dupli :: [a] -> [a]
> dupli [] = []
> dupli (x:xs) = [x,x] ++ (dupli xs)

> dupli' :: [a] -> [a]
> dupli' [] = []
> dupli' (x:xs) = x:x:dupli' xs

Or using the list monado:

> dupli'' :: [a] -> [a]
> dupli'' xs = xs >>= (\x -> [x,x])

> dupli''' :: [a] -> [a]
> dupli''' lst = [x| x<- lst, _ <- [1,2]]
