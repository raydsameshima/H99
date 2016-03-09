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

> data HufTree a = Leaf (a, Int)
>                  | Node Int (HufTree a) (HufTree a)
>                  deriving (Show)

> fromList :: [(a,Int)] -- sorted 
>          -> HufTree a
> fromList = fromList' . hSort
>
> fromList' :: [(a,Int)] -> HufTree a
> fromList' [(c,n),(d,m)] = Node (n+m) (Leaf (d,m)) (Leaf (c,n))
> fromList' lst = undefined

> hSort :: [(a, Int)] -> [(a, Int)]
> hSort [] = []
> hSort ((c,n):rest) = smaller ++ ((c,n):greater)
>   where
>     smaller = hSort [(d,m) | (d,m) <- rest, m <= n]
>     greater = hSort [(d,m) | (d,m) <- rest, m >  n]

> huffman :: [(Char, Int)] -> [(Char, String)]
> huffman = huffman' . hSort

> huffman' :: [(Char, Int)]    -- already sorted
>          -> [(Char, String)]
> huffman' [] = []
> huffman' [(c,_)] = [(c,"0")]
> huffman' [(c,n),(d,m)] = [(c,"0"), (d,"1")]
> huffman' lst = undefined
