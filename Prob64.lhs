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
> toList = takeWhile (any isJust) . filled
>   where
>     filled
>       :: Tree a -> [[Maybe a]]
>     filled Empty          = repeat [Nothing]
>     filled (Branch x l r) = [Just x] : zipWith (++) (filled l) (filled r)

*Prob64 Data.Maybe> toList tree64 
[[Just 'n']
,[Just 'k',Just 'u']
,[Just 'c',Just 'm',Just 'p',Nothing]
,[Just 'a',Just 'h',Nothing, Nothing,Nothing,Just 's',Nothing]
,[Nothing, Nothing, Just 'g',Nothing,Nothing,Nothing, Nothing,Just 'q',Nothing,Nothing]
,[Nothing, Nothing, Just 'e',Nothing,Nothing,Nothing, Nothing,Nothing, Nothing,Nothing,Nothing,Nothing]
]

So we can assign the depth:

*Prob64 Turtle> zipWith (\n ts -> [(n, t) | t <- ts]) [1..] it
[[(1,Just 'n')]
,[(2,Just 'k'),(2,Just 'u')]
,[(3,Just 'c'),(3,Just 'm'),(3,Just 'p'),(3,Nothing)]
,[(4,Just 'a'),(4,Just 'h'),(4,Nothing),(4,Nothing),(4,Nothing),(4,Just 's'),(4,Nothing)]
,[(5,Nothing),(5,Nothing),(5,Just 'g'),(5,Nothing),(5,Nothing),(5,Nothing),(5,Nothing),(5,Just 'q'),(5,Nothing),(5,Nothing)]
,[(6,Nothing),(6,Nothing),(6,Just 'e'),(6,Nothing),(6,Nothing),(6,Nothing),(6,Nothing),(6,Nothing),(6,Nothing),(6,Nothing),(6,Nothing),(6,Nothing)]
]

> depths :: Tree a -> [[Maybe (a, Int)]]
> depths t = t'
>   where
>     t'' = zipWith (\n ts -> [(n,t) | t <- ts]) [1..] $ toList t
>     t' = map (map helper) t''
>     helper :: (Int, Maybe a) -> Maybe (a, Int)
>     helper (_, Nothing) = Nothing
>     helper (d, Just n)  = Just (n, d) 

> inOrder 
>   :: [[Maybe a]] -> [Maybe a]
> inOrder [[x]] = [x]
> inOrder [xs,ys] = (filter isJust $ inOrder' xs ys)
> inOrder (xs:ys:zs) = inOrder $ filter isJust (inOrder' xs ys) : zs 

> inOrder' 
>   :: [a] -> [a] -> [a]
> inOrder' [] bs = bs
> inOrder' as [] = as
> inOrder' (a:as) (b:bs) = b:a: inOrder' as bs

*Prob64 Turtle> zipWith (\x (Just (n,d)) -> Just (n,x,d)) [1..] . inOrder . depths $ tree64 
[Just ('a',1,4),Just ('c',2,3),Just ('e',3,6),Just ('g',4,5),Just ('h',5,4),Just ('k',6,2),Just ('m',7,3),Just ('n',8,1),Just ('p',9,3),Just ('q',10,5),Just ('s',11,4),Just ('u',12,2)]

> type X = Int
> type Depth = Int
> nodesAndCoordinates :: Tree a -> [(a, X, Depth)]
> nodesAndCoordinates ts = map fromJust js
>   where
>     js = zipWith helper [1..] ts'
>     helper x (Just (n,d)) = Just (n,x,d)
>     ts' = inOrder . depths $ ts

... ok let us cheat.

> type Pos = (Int, Int)
> layout
>   :: Tree a -> Tree (a, Pos)
> layout t = fst (aux 1 1 t)
>   where
>     aux :: Int -> Int -> Tree a -> (Tree (a, Pos), Int)
>     aux x _ Empty          = (Empty,                    x)
>     aux x y (Branch a l r) = (Branch (a, (x',y)) l' r', x'')
>       where
>         (l', x')  = aux x      (y+1) l
>         (r', x'') = aux (x'+1) (y+1) r

*Prob64 Turtle> nodesAndCoordinates tree64 
[('a',1,4),('c',2,3),('e',3,6),('g',4,5),('h',5,4),('k',6,2),('m',7,3),('n',8,1),('p',9,3),('q',10,5),('s',11,4),('u',12,2)]

*Prob64 Turtle> layout tree64 
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) (Branch ('a',(1,4)) Empty Empty) (Branch ('h',(5,4)) (Branch ('g',(4,5)) (Branch ('e',(3,6)) Empty Empty) Empty) Empty)) (Branch ('m',(7,3)) Empty Empty)) (Branch ('u',(12,2)) (Branch ('p',(9,3)) Empty (Branch ('s',(11,4)) (Branch ('q',(10,5)) Empty Empty) Empty)) Empty)

