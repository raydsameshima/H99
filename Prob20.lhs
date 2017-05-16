Prob20.lhs

> module Prob20 where

Remove the K'th element from a list.

> removeAt 
>   :: Int -> [a] -> (a,[a]) 
> removeAt n xs
>   | n < 0     = error "removeAt: negative input"
>   | otherwise = (xs !! (n-1), xs')
>   where 
>     xs' = (take (n-1) xs) ++ (drop n xs)

A simple recursice solution, without error handling:

> removeAt' 
>   :: Int -> [a] -> (a, [a])
> removeAt' n _ 
>   | n < 1 = error "removeAt': negative input"
> removeAt' 1 (x:xs) = (x, xs)
> removeAt' n (x:xs) = (left, x:right)
>   where 
>     (left, right) = removeAt' (n-1) xs

> removeAtMaybe 
>   :: Int -> [a] -> (Maybe a, [a])
> removeAtMaybe _ [] = (Nothing, [])
> removeAtMaybe n lst
>   | n < 0 || n > length lst = (Nothing, lst)
>   | otherwise               = (Just (lst !! (n-1)), lst')
>   where 
>     lst' = (take (n-1) lst) ++ (drop n lst)
