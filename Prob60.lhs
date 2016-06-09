Prob60.lhs

> module Prob60 where
> import Prob54(Tree(..))

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
[ [Empty]                              -- 0
, [Branch 'x' Empty Empty]             -- 1
, [Branch 'x' Empty                    -- 2
              (Branch 'x' Empty Empty)
  ,Branch 'x' (Branch 'x' Empty Empty) 
              Empty
  ]
, [Branch 'x' (Branch 'x' Empty Empty) -- 3
              (Branch 'x' Empty Empty)
  ]
]

Maximum number of nodes in a height-balanced tree of height h.
1, 2, 4, 8, ...
  *Prob60> map minHeight [1..16]
  [1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5]

> maxNodes :: Int -> Int
> maxNodes h = 2^h -1

Minimum height of a height-balanced tree of n nodes.
  *Prob60> map ceiling [-1.1, -0.9, -0.4, 0.4, 0.7, 1.3]
  [-1,0,0,1,1,2]

> minHeight :: Int -> Int
> minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1) 

Minimum number of nodes in a height-balanced tree of height h.

> minNodes :: Int -> Int
> minNodes h = (fibs !! (h+2)) -1

Maximum height of a height-balanced tree of n nodes.

> maxHeight :: Int -> Int
> maxHeight n = (length $ takeWhile (<= n+1) fibs) -3

> fibs :: [Int]
> fibs = 0:1: zipWith (+) fibs (tail fibs)

Here we plant the trees we want.

> hbalTreeNodes :: a -> Int -> [Tree a]
> hbalTreeNodes x n =
>   [t | h <- [(minHeight n) .. (maxHeight n)], t <- baltree h n]
>   where
>     baltree 0 _ = [Empty]
>     baltree 1 _ = [Branch x Empty Empty] 
>     baltree h n =
>       [ Branch x l r 
>       | (hl,hr) <- [(h-2,h-1),(h-1,h-1),(h-1,h-2)]
>       , let min_nl = min (minNodes hl) (n-1- (maxNodes hr))
>       , let max_nl = max (minNodes hl) (n-1- (maxNodes hr))
>       , nl <- [min_nl .. max_nl]
>       , let nr = n-1-nl
>       , l <- baltree hl nl
>       , r <- baltree hr nr
>       ]

  *Prob60> map (hbalTreeNodes 'x') [0..3]
  [ [Empty]                               -- 0
  , [Branch 'x' Empty Empty]              -- 1
  , [ Branch 'x' Empty                    -- 2
                 (Branch 'x' Empty Empty)
    , Branch 'x' (Branch 'x' Empty Empty) 
                 (Branch 'x' Empty Empty)
    , Branch 'x' (Branch 'x' Empty Empty) 
                 (Branch 'x' Empty Empty)
    , Branch 'x' (Branch 'x' Empty Empty) 
                 Empty
    ]
  , [ Branch 'x' Empty                    -- 3
                 (Branch 'x' Empty Empty)
    , Branch 'x' Empty 
                 (Branch 'x' Empty Empty)
    , Branch 'x' (Branch 'x' Empty Empty) 
                 (Branch 'x' Empty Empty)
    , Branch 'x' (Branch 'x' Empty Empty) 
                 Empty
    , Branch 'x' (Branch 'x' Empty Empty) 
                 Empty
    ]
  ]
  *Prob60> map (length . hbalTreeNodes 'x') [0..10]
  [1,1,4,5,119,65,50,352488,200667,112318,61739]
