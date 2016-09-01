Prob63.lhs

> module Prob63 where
> import Prob54(Tree(..))

Construct a complete binary tree

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e., 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". 
This means that in a level order tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. 
For every node X with address A the following property holds: 
The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. 
This fact can be used to elegantly construct a complete binary tree structure.

> completeBinaryTree = cbt 'x'
> cbt :: a -> Int -> Tree a
> cbt = undefined

