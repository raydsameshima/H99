Prob63.lhs

> module Prob63 where

> import Data.List (group)
> import Test.QuickCheck

> import Prob54(Tree(..))
> import Prob62(atLevel)

> tree4 = Branch 1 (Branch 2 Empty 
>                            (Branch 4 Empty 
>                                      Empty))
>                  (Branch 2 Empty 
>                            Empty)
> fullTree = Branch 1 (Branch 2 Empty 
>                               Empty)
>                     (Branch 2 Empty 
>                               Empty)

Construct a complete binary tree
(See also https://xlinux.nist.gov/dads/HTML/completeBinaryTree.html)

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e., 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". 
This means that in a level order tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. 
For every node X with address A the following property holds: 
The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. 
This fact can be used to elegantly construct a complete binary tree structure.

> aTestTree = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)

The follwoing implementation is from
  
  "...  a node at index i has children at indexes 2i and 2i+1 ..."
  https://xlinux.nist.gov/dads/HTML/completeBinaryTree.html

> cbt 
>   :: Int -> Tree Char
> cbt n = f 1
>   where
>     f :: Int -> Tree Char
>     f i
>       | i > n     = Empty
>       | otherwise = Branch 'x' (f $ 2*i)
>                                (f $ 2*i + 1)

*Prob63> cbt 12
Branch 'x' (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                                   (Branch 'x' Empty Empty)) 
                       (Branch 'x' (Branch 'x' Empty Empty) 
                                   (Branch 'x' Empty Empty))) 
           (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                                   Empty) 
                       (Branch 'x' Empty 
                                   Empty))
Since
  2^0 < 2^1 < 2^2 < 2^3 < 12 < 2^4
the steps of internal recursion will be
  f 1
    =
  Branch 'x' (f 2)
             (f 3)
    =
  Branch 'x' (Branch 'x' (f 4)
                         (f 5))
             (Branch 'x' (f 6)
                         (f 7))
    =
  Branch 'x' (Branch 'x' (Branch 'x' (f 8)   --> (Branch 'x' Empty Empty)
                                     (f 9))  --> (Branch 'x' Empty Empty) 
                         (Branch 'x' (f 10)  --> (Branch 'x' Empty Empty) 
                                     (f 11)) --> (Branch 'x' Empty Empty)
             (Branch 'x' (Branch 'x' (f 12)  --> (Branch 'x' Empty Empty)
                                     (f 13)) --> Empty
                         (Branch 'x' (f 14)  --> Empty
                                     (f 15)) --> Empty

That is, the i's are the indices, in a sense:

> cbt'
>   :: Int -> Tree Int
> cbt' n = f 1
>   where
>     f :: Int -> Tree Int
>     f i
>       | i > n     = Empty
>       | otherwise = Branch i (f $ 2*i)
>                              (f $ 2*i + 1)

*Prob63> cbt' 12
Branch 1 (Branch 2 (Branch 4 (Branch 8 Empty Empty) 
                             (Branch 9 Empty Empty)) 
                   (Branch 5 (Branch 10 Empty Empty) 
                             (Branch 11 Empty Empty))) 
         (Branch 3 (Branch 6 (Branch 12 Empty Empty) 
                             Empty) 
                   (Branch 7 Empty 
                             Empty))

Let us follow the solution:
levels' returns the tree structure in list where Empty becomes False:

> levels' 
>   :: Tree a -> [[Bool]]
> levels' = takeWhile or . filled 
>   where
>     filled -- infinite sequence ! 
>       :: Tree a -> [[Bool]]
>     filled Empty = repeat [False]
>     filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r)

*Prob63 Turtle> levels' tree4 
[[True],[True,True],[False,True,False,False]]

tree4 = Branch 1 (Branch 2 Empty 
                           (Branch 4 Empty 
                                     Empty))
                 (Branch 2 Empty 
                           Empty)

For a CBT, the last list of (levels' t) must be the following form:
  [True .. True, False .. False]
so, lastFilled' returns either 
  [True, False] or [True]
for CBT.

> lastFilled' 
>   :: Tree a -> [Bool]
> lastFilled' = map head . group . last . levels'

Therefore, the bottom of a CBT t has True at the head in (lastFilled' t) 
and (lastFilled' t) is either sigleton or two elements (see above).

> lastProper'
>   :: Tree a ->  Bool
> lastProper' t = head (lastFilled' t) && length (lastFilled' t) < 3

Equipped with these helper functions, isCBT is the following form:

> isCBT 
>   :: Tree a -> Bool
> isCBT Empty = True
> isCBT t = and $ (lastProper' t) : zipWith (==) lengths powers
>   where
>     lengths, powers :: [Int]
>     lengths = map (length . filter id) $ init $ levels' t
>     powers = [2^n | n <- [0..]] -- infinite !

lengths is the numbers of non-empty nodes of each depth (but the lowest).
Since we've already replaced all non-empty element by True (by levels'),
we can simply filter them by the elements.
So this implementation is basically, when both
  the bottom is left-aligned 
and
  the rest parts is complete binary tree
then returns True, otherwise False.
  
QuickCheck: 
Since the following function should be True-const:

> prop_isCBT n = isCBT . cbt' 

*Prob63 Turtle> quickCheck prop_isCBT 
+++ OK, passed 100 tests.

