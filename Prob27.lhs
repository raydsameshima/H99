Prob27.lhs

> module Prob27 where

Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2,3 and 4 persons?
Write a function that generates all the possibilities and return them in a list.

b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

> combination :: Int -> [a] -> [([a], [a])]
> combination 0 xs = [([], xs)]
> combination n [] = []
> combination n (x:xs) = ts ++ ds
>   where
>     ts = [(x:ys, zs) | (ys, zs) <- combination (n-1) xs]
>     ds = [(ys, x:zs) | (ys, zs) <- combination n     xs]

> group :: [Int] -> [a] -> [[[a]]]
> group []     _  = [[]]
> group (n:ns) xs =
>   [ g:gs | (g, rs) <- combination n xs
>          , gs      <- group ns rs ]

