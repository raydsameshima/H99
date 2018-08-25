Prob61.lhs

> module Prob61 where
> import Prob54 (Tree(..))

An example tree

> tree4 :: Tree Int
> tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
>                  (Branch 2 Empty Empty)

Count the leaves of a binary tree

A leaf is a node with no successors. 
Write a predicate count_leaves/2 to count them.

> countLeaves 
>   :: Tree a -> Int
> countLeaves Empty = 0
> countLeaves (Branch _ Empty Empty) = 1
> countLeaves (Branch _ leftT righT) = (countLeaves leftT) + (countLeaves righT)

  *Prob61> countLeaves tree4 
  2

(Problem 61A)
Collect the leaves of a binary tree in a list

A leaf is a node with no successors. 
Write a predicate leaves/2 to collect them in a list.

> leaves
>   :: Tree a -> [a]
> leaves Empty = []
> leaves (Branch x Empty Empty) = [x]
> leaves (Branch x leftT righT) = (leaves leftT) ++ (leaves righT) 

To avoid (++), here is an alternative:

> leaves'
>    :: Tree a -> [a]
> leaves' t = f t []
>   where
>     f Empty                  xs = xs
>     f (Branch x Empty Empty) xs = x:xs
>     f (Branch _ leftT righT) xs = f leftT (f righT xs)
