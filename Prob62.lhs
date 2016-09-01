Prob62.lhs

> module Prob62 where
> import Prob54 (Tree(..))

An example tree

> tree4 :: Tree Int
> tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
>                  (Branch 2 Empty Empty)

Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty successors. 
Write a predicate internals/2 to collect them in a list.

> internals :: Tree a -> [a]
> internals Empty = []
> internals (Branch _ Empty Empty) = []
> internals (Branch x leftT righT) = x:(internals leftT)++(internals righT)

An alternative solution with (:) only:

> internals' t = f t []
>   where
>     f Empty                  xs = xs
>     f (Branch _ Empty Empty) xs = xs
>     f (Branch x leftT righT) xs = x: (f leftT (f righT xs))

  *Prob62> internals tree4
  [1,2]
  *Prob62> internals' tree4
  [1,2]

(Problem 62B)
Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the node has length N-1. 
The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.

> atLevel :: Tree a -> Int -> [a]
> atLevel _ n 
>   | n <= 0 = []
> atLevel Empty          _ = []
> atLevel (Branch x _ _) 1 = [x]
> atLevel (Branch x l r) n = (atLevel l (n-1)) ++ (atLevel r (n-1))
