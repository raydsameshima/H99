Prob60.lhs

> module Prob60 where
>
> import Prob54 (Tree(..))
> import Prob59 (hbalTree)
> import Data.Maybe (fromJust) -- :: Maybe a -> a
> import Data.List (findIndex) -- :: (a -> Bool) -> [a] -> Maybe Int

Construct height-balanced binary trees with a given number of nodes.

example in Haskell:
  *Main> length $ hbalTreeNodes 'x' 15
  1553
  *Main> map (hbalTreeNodes 'x') [0..3]
  [ [Empty]                              -- n=0
  , [Branch 'x' Empty Empty]             -- n=1
  , [Branch 'x' Empty                    -- n=2
                (Branch 'x' Empty Empty)
    ,Branch 'x' (Branch 'x' Empty Empty) 
                Empty
    ]
  , [Branch 'x' (Branch 'x' Empty Empty) -- n=3
                (Branch 'x' Empty Empty)
    ]
  ]
  *Main> map length it 
  [1,1,2,1]

Consider a height-balanced binary tree of a given height h.
What is the maximum number of nodes it can contain?
Clearly, 
  maxNodes h = 1+2+4+..+2^(h-1)
             = 2^h -1

However, what is the minimum number (minNodes h)? 
This question is more difficult. 
Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height h. 
Since the left sub-tree of height h is given by that of (h-1), and the right sub-tree is that of (h-2) to maintain height-balanced, the recursive definition is something like the follwoings:

> minNodesF :: Int -- given hight 
>           -> Int
> minNodesF 0 = 0
> minNodesF 1 = 1
> minNodesF h = 1 + (minNodesF (h-1)) + (minNodesF (h-2))

Or more elegantly (from the solution),

> minNodes :: Int -- given height
>          -> Int
> minNodesSeq = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq) 
> minNodes = (minNodesSeq !!)

They give us the same numbers:

  *Prob60> take 10 minNodesSeq 
  [0,1,2,4,7,12,20,33,54,88]
  *Prob60> take 10 $ map minNodesF [0..]
  [0,1,2,4,7,12,20,33,54,88]

On the other hand, we might ask: what is the maximum height h a height-balanced binary tree with given n nodes can have? 
Write a function maxHeight that computes this.

> maxHeight :: Int -- given nodes 
>           -> Int
> maxHeight n = (fromJust $ findIndex (> n) minNodesSeq) - 1

The minimum height is given by

> minHeight :: Int -- given nodes
>           -> Int
> minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)

Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. 
Find out how many height-balanced trees exist for n = 15.

> hbalTreeNodes :: a -> Int -> [Tree a]
> hbalTreeNodes _ 0 = [Empty]
> hbalTreeNodes x n = concatMap toFilteredTrees [(minHeight n) .. (maxHeight n)]
>   where 
>     toFilteredTrees h = filter ((n ==) . countNodes) $ hbalTree x h
>     -- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
> 
> countNodes :: Tree a -> Int
> countNodes Empty = 0
> countNodes (Branch _ l r) = (countNodes l) + (countNodes r) + 1








Maximum number of nodes in a height-balanced tree of height h.

> maxNodes' :: Int -> Int
> maxNodes' h = 2^h -1

Minimum height of a height-balanced tree of n nodes.
Here is an example of ceiling function:
  *Prob60> map ceiling [-1.1, -0.9, -0.4, 0.4, 0.7, 1.3]
  [-1,0,0,1,1,2]

> minHeight' :: Int -> Int
> minHeight' n = ceiling $ logBase 2 $ fromIntegral (n+1) 

  *Prob60> map minHeight [1..16]
  [1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5]

Minimum number of nodes in a height-balanced tree of height h.
But why it is fib?

> minNodes' :: Int -> Int
> minNodes' h = (fibs' !! (h+2)) -1

Maximum height of a height-balanced tree of n nodes.

> maxHeight' :: Int -> Int
> maxHeight' n = (length $ takeWhile (<= n+1) fibs') -3

> fibs' :: [Int]
> fibs' = 0:1: zipWith (+) fibs' (tail fibs')

Here we plant the trees we want.

> hbalTreeNodes' :: a -> Int -> [Tree a]
> hbalTreeNodes' x n =
>   [t | h <- [(minHeight' n) .. (maxHeight' n)], t <- baltree h n]
>   where
>     baltree 0 _ = [Empty]
>     baltree 1 _ = [Branch x Empty Empty] 
>     baltree h n =
>       [ Branch x l r 
>       | (hl,hr) <- [(h-2,h-1),(h-1,h-1),(h-1,h-2)]
>       , let min_nl = max (minNodes' hl) (n-1- (maxNodes' hr))
>       , let max_nl = min (minNodes' hl) (n-1- (maxNodes' hr))
>       , nl <- [min_nl .. max_nl]
>       , let nr = n-1-nl
>       , l <- baltree hl nl
>       , r <- baltree hr nr
>       ]


