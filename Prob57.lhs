Prob57.lhs

> module Prob57 where

Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

> import Prob54 (Tree(..))
> import Prob56 (isSym)
>
> consTree :: (Ord a) => [a] -> Tree a
> consTree [] = Empty
> consTree (n:ns) = Branch n left right
>   where
>     left  = consTree [m| m <- ns, m < n]
>     right = consTree [m| m <- ns, m > n]

  *Prob57> consTree [3,2,5,7,1]
  Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
  *Prob57> isSym it
  True
  *Prob57> consTree [5,3,18,1,4,12,21]
  Branch 5 (Branch 3 (Branch 1 Empty Empty) (Branch 4 Empty Empty)) (Branch 18 (Branch 12 Empty Empty) (Branch 21 Empty Empty))
  *Prob57> isSym it
  True

From the solution,

> add2Tree :: (Ord a) => a -> Tree a -> Tree a
> add2Tree x Empty = Branch x Empty Empty
> add2Tree x t@(Branch y l r)
>   = case x `compare` y of
>       LT -> Branch y (add2Tree x l) r
>       GT -> Branch y l              (add2Tree x r)
>       EQ -> t
> 
> construct :: (Ord a) => [a] -> Tree a
> construct xs = foldl (flip add2Tree) Empty xs
