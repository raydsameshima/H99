Prob64.lhs

> module Prob64 where

> import Data.Maybe

> import Prob54(Tree(..))

Given a binary tree as the usual Prolog term t(X,L,R) (or nil).
As a preparation for drawing the tree, a layout algorithm is required to determine
the position of each node in a rectangular grid.
Several layout methods are conceivable, one of them is shown in the illustration below:

  01 02 03 04 05 06 07 08 09 10 11 12
1                       n
  
2                 k                 u
  
3     c              m     p
  
4  a           h                 s
  
5           g                 q
  
6        e

In this layout strategy, the position of a node v is obtained by the following two rules:
  x(v) is equal to the position of the node v in the inorder sequence
  y(v) is equal to the depth of the node v in the tree

Write a function to annotate each node of the tree with a position, where (1,1) in
the top left corner or the rectangle bounding the drawn tree.

Here is the example tree from the above illustration:

> tree64 = Branch 'n'
>                 (Branch 'k'
>                         (Branch 'c'
>                                 (Branch 'a' Empty Empty)
>                                 (Branch 'h'
>                                         (Branch 'g'
>                                                 (Branch 'e' Empty Empty)
>                                                 Empty
>                                         )
>                                         Empty
>                                 )
>                         )
>                         (Branch 'm' Empty Empty)
>                 )
>                 (Branch 'u'
>                         (Branch 'p'
>                                 Empty
>                                 (Branch 's'
>                                         (Branch 'q' Empty Empty)
>                                         Empty
>                                 )
>                         )
>                         Empty
>                 )

Let us start mimic the levels' from Prob63:

> toList
>   :: Tree a -> [[Maybe a]]
> toList = takeWhile (or . map (isJust)) . filled
>   where
>     filled :: Tree a -> [[Maybe a]]
>     filled Empty = repeat [Nothing]
>     filled (Branch x l r) = [Just x] : zipWith (++) (filled l) (filled r)

*Prob64 Data.Maybe> toList tree64 
[[Just 'n']
,[Just 'k',Just 'u']
,[Just 'c',Just 'm',Just 'p',Nothing]
,[Just 'a',Just 'h',Nothing,Nothing,Nothing,Just 's',Nothing]
,[Nothing,Nothing,Just 'g',Nothing,Nothing,Nothing,Nothing,Just 'q',Nothing,Nothing]
,[Nothing,Nothing,Just 'e',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
]



















































