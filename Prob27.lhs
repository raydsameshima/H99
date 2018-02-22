Prob27.lhs

> module Prob27 where

Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2,3 and 4 persons?
Write a function that generates all the possibilities and return them in a list.

  *Prob27> length . group [2,3,4] $ ['a' .. 'i']
  1260
  *Prob27> length . group [2,2,5] $ ['a' .. 'i']
  756

b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

Base cases; 0 choice in any input is a singleton, and any choice from empty is empty.
Induction step; using the fact
  comb(n,k) = comb(n-1, k-1) + comb(n-1, k)
we divide the cases: either x (head element) is in the first or second.

> combination 
>   :: Int -> [a] -> [([a], [a])]
> combination 0 xs     = [([], xs)]
> combination _ []     = []
> combination n (x:xs) = ts ++ ds
>   where
>     ts = [(x:ys, zs)   | (ys, zs) <- combination (n-1) xs]
>     ds = [(ys  , x:zs) | (ys, zs) <- combination n     xs]
  
  *Prob27> length . combination 3 $ [1..6]
  20
  *Prob27> 6*5*4 `div` (1*2*3)
  20

> group 
>   :: [Int] -> [a] -> [[[a]]]
> group []     _  = [[]]
> group (n:ns) xs =
>   [ g:gs | (g, rs) <- combination n xs
>          , gs      <- group ns rs ]

