Prob18.lhs

> module Prob18 where

Extract a slice from a list.
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limit included).
Start counting the elements with 1.

> slice 
>   :: [a] -> Int -> Int -> [a]
> slice [] _ _ = []
> slice xs i k 
>   | i > 0 && i <= k = drop (i-1) (take k xs)
>   | otherwise       = [] 

A solution using list comprehension:

> slice' 
>   :: [a] -> Int -> Int -> [a]
> slice' xs i k = [x | (x,j) <- zip xs [1..k], i <= j]

> slice'' 
>   :: [a] -> Int -> Int -> [a]
> slice'' xs i k = map fst $ filter ((>= i) . snd) $ zip xs [1 .. k]
