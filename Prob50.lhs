Prob50.lhs

> module Ptob50 where

Huffman codes.

We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. 
Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. 
In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. 
The task shall be performed by the predicate huffman/2 defined as follows:

  > huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

see
http://www.snap-tck.com/room03/c02/comp/comp032.html

> data HTree a 
>   = Leaf (a, Int)
>   | Node Int (HTree a) (HTree a)
>   deriving (Show)

OK, we have a data type for Huffman tree, and hSort:

> hSort :: [(a, Int)] -> [(a, Int)]
> hSort [] = []
> hSort ((c,n):rest) = smaller ++ ((c,n):greater)
>   where
>     smaller = hSort [(d,m) | (d,m) <- rest, m <= n]
>     greater = hSort [(d,m) | (d,m) <- rest, m >  n]

> converter :: [(a, Int)] -> [HTree a]
> converter lst = map toLeaf lst
>   where
>     toLeaf :: (a, Int) -> HTree a
>     toLeaf (c,n) = Leaf (c,n)     

  *Ptob50> hSort [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  [('f',5),('e',9),('c',12),('b',13),('d',16),('a',45)]
  *Ptob50> converter it
  [Leaf ('f',5),Leaf ('e',9),Leaf ('c',12),Leaf ('b',13),Leaf ('d',16),Leaf ('a',45)]

> weight :: HTree a -> Int
> weight (Leaf (c,n)) = n
> weight (Node n _ _) = n

> fromList :: [(a,Int)] -> HTree a
> fromList = fromList' . sort' . converter

> fromList' :: [HTree a] -> HTree a
> fromList' [l] = l
> fromList' [l1,l2] 
>   | n1 <= n2  = Node (n1+n2) l1 l2
>   | otherwise = Node (n1+n2) l2 l1
>   where
>     n1 = weight l1
>     n2 = weight l2
> fromList' (l1:l2:rest) = fromList' $ sort' ((fromList' [l1,l2]) : rest) 
>
> sort' [] = []
> sort' (l1:rest) = sm ++ (l1:gt)
>   where
>     sm = sort' [l | l <- rest, weight l <= weight l1]
>     gt = sort' [l | l <- rest, weight l >  weight l1]

  *Ptob50> fromList [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  Node 100 (Leaf ('a',45)) (Node 55 (Node 25 (Leaf ('c',12)) (Leaf ('b',13))) (Node 30 (Node 14 (Leaf ('f',5)) (Leaf ('e',9))) (Leaf ('d',16))))
