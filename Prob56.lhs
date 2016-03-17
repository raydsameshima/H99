Prob56.lhs

> module Prob56 where

Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. 
Write a predicate symmetric/1 to check whether a given binary tree is symmetric. 
Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. 
We are only interested in the structure, not in the contents of the nodes.

> import Prob54 (Tree(..))
>
> isMir :: Tree a -> Tree a -> Bool
> isMir Empty Empty = True
> isMir (Branch _ l1 r1) (Branch _ l2 r2) = (l1 `isMir` r2) && (r1 `isMir` l2)
> isMir _     _     = False

> isSym :: Tree a -> Bool
> isSym Empty = True
> isSym (Branch _ left right) = left `isMir` right
> -- isSym t = t `isMir` t
