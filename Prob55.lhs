Prob55.lhs

> module Prob55 where

Construct completely balanced binary trees.

In a completely balanced binary tree, the following property holds for every node: 
The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. 
The predicate should generate all solutions via backtracking. 
Put the letter 'x' as information into all nodes of the tree.

> import Prob54 (Tree(..))

> cbalTree :: Int -> [Tree Char]
> cbalTree 0 = [Empty] -- base case
> cbalTree n = let (q, r) = (n-1) `quotRem` 2 -- n-1 = 2*q + r
>   in [Branch 'x' left right | i     <- [q .. q+r],
>                               left  <- cbalTree i,
>                               right <- cbalTree (n-1 -i)]

  *Prob55> map (length . cbalTree) [1..17]
  [1,2,1,4,4,4,1,8,16,32,16,32,16,8,1,16,64]

For given n>0, first Branch is filled.
Next, by dividing we get 
  (n-1) = 2*q + r
If r=0, then i=q, and both left and right sub-trees has q x's.
Else if r=1, we have two choices for sub-trees: 
  (i,(n-1)-i) = (q, q+1) or (q+1,q)

A slightly more efficient version of this solution, which never creates the same tree twice:

> cbalTree1 :: Int -> [Tree Char]
> cbalTree1 0 = [Empty]
> cbalTree1 n =
>   if (n `mod` 2 == 1) then
>       [Branch 'x' l r | l <- subtree (n-1), r <- subtree (n-1)] 
>   else
>       concat [ [Branch 'x' l r, Branch 'x' r l]
>              | l <- subtree (n-1), r <- subtree n]
>   where 
>     subtree n = cbalTree1 (n `div` 2)

  *Prob55> cbalTree 4 
  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)]
  *Prob55> cbalTree1 4
  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)]

