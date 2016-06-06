Prob60.lhs

> module Prob60 where

Construct height-balanced binary trees with a given number of nodes.

Consider a height-balanced binary tree of height H. 
What is the maximum number of nodes it can contain?

Clearly, MaxN = 2**H - 1. 
However, what is the minimum number MinN? 
This question is more difficult. 
Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H. 
On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? 
Write a function maxHeight that computes this.
Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. 
Find out how many height-balanced trees exist for N = 15.

example in Haskell:

*Main> length $ hbalTreeNodes 'x' 15
1553
*Main> map (hbalTreeNodes 'x') [0..3]
[[Empty],
 [Branch 'x' Empty Empty],
 [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
 [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]


