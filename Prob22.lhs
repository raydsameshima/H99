Prob22.lhs

> module Prob22 where

Create a list containing all integers within a given range.

> range :: Int -> Int -> [Int]
> range n1 n2
>   | n1 <= n2  = take (n2-n1+1) [n1..]
>   | otherwise = take (n1-n2+1) [n1, n1-1..]

Without using a reverse function

  range n m
    | n == m    = [n]
    | n < m     = n:(range (n+1) m)
    | otherwise = n:(range (n-1) m) 

> range' :: Int -> Int -> [Int]
> range' min max
> --  | min > max = []
>   | min > max = [min, min-1 .. max]
>   | otherwise = [min..max]
