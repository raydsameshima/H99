Prob59.lhs

> module Prob59 where

Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: 
The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and the given maximum height.

> import Prob54(Tree(..))
>
> something :: [Tree Char]
> something =
>   [
>     Branch 'x' (Branch 'x' Empty Empty) 
>                (Branch 'x' Empty (Branch 'x' Empty Empty)),
>     Branch 'x' (Branch 'x' Empty Empty) 
>                (Branch 'x' (Branch 'x' Empty Empty) Empty),
>     Branch 'x' (Branch 'x' Empty Empty) 
>                (Branch 'x' (Branch 'x' Empty Empty) 
>                            (Branch 'x' Empty Empty)),
>     Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) 
>                (Branch 'x' Empty Empty)
>   ]

> hbalTree :: a -> Int -> [Tree a]
> hbalTree x 0 = [Empty]
> hbalTree x 1 = [Branch x Empty Empty]
> hbalTree x n =
>   [ Branch x l r
>   | (nl, nr) <- [(n-2,n-1),(n-1,n-1),(n-1,n-2)]
>   , l <- hbalTree x nl, r <- hbalTree x nr ]

  *Prob59> take 4 $ hbalTree 'x' 3
  [Branch 'x' (Branch 'x' Empty Empty) 
              (Branch 'x' Empty (Branch 'x' Empty Empty))
  ,Branch 'x' (Branch 'x' Empty Empty) 
              (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
  ,Branch 'x' (Branch 'x' Empty Empty) 
              (Branch 'x' (Branch 'x' Empty Empty) Empty)
  ,Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) 
              (Branch 'x' Empty (Branch 'x' Empty Empty))
  ]
