Prob59.lhs

> module Prob59 where
> import Prob54(Tree(..))

Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: 
The height of its left subtree and the height of its right subtree are 
  almost equal, 
which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and the given maximum height.

> something :: [Tree Char]
> something =
>   [ Branch 'x' (Branch 'x' Empty Empty) 
>                (Branch 'x' Empty (Branch 'x' Empty Empty))
>   , Branch 'x' (Branch 'x' Empty Empty) 
>                (Branch 'x' (Branch 'x' Empty Empty) Empty)
>   , Branch 'x' (Branch 'x' Empty Empty) 
>                (Branch 'x' (Branch 'x' Empty Empty) 
>                            (Branch 'x' Empty Empty))
>   , Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) 
>                (Branch 'x' Empty Empty)
>   ]
> -- h :: Int is the height.
> hbalTree :: a -> Int -> [Tree a]
> hbalTree x 0 = [Empty]
> hbalTree x 1 = [Branch x Empty Empty]
> hbalTree x h =
>   [ Branch x l r
>   | (hl,hr) <- [(h-2,h-1),(h-1,h-1),(h-1,h-2)] -- height-balanced
>   , l <- hbalTree x hl, r <- hbalTree x hr ]

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

If we want to avoid recomputing lists of trees (at the cost of extra space), we can use a similar structure to the common method for computation of all the Fibonacci numbers:


> hbalTree' x h = trees !! h
>   where
>     trees = [Empty] : [Branch x Empty Empty] :
>             zipWith combine (tail trees) trees -- this tail is safe.
>     combine ts shortts = 
>       [ Branch x l r
>       | (ls, rs) <- [(shortts, ts), (ts, ts), (ts, shortts)]
>       , l <- ls, r <- rs ]                   

