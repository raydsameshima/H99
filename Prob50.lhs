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

> example 
>   :: [(Char,Int)]
> example = [(' ',7),('a',4),('e',4),('f',3),('h',2)
>           ,('i',2),('l',2),('m',2),('n',2),('o',1)
>           ,('p',1),('r',1),('s',2),('t',2),('u',1)
>           ,('x',1)
>           ]

See the following link
  http://www.snap-tck.com/room03/c02/comp/comp032.html
Let us construct Huffman tree:

> data HTree a -- Huffman tree
>   = Leaf (a, Int)
>   | Node Int (HTree a) (HTree a)
>   deriving (Show)

> sortByFreq 
>   :: [(a, Int)] -> [(a, Int)]
> sortByFreq []           = []
> sortByFreq ((c,n):rest) = smaller ++ ((c,n):greater)
>   where -- For input, quick sort
>     smaller = sortByFreq [(d,m) | (d,m) <- rest, m <= n]
>     greater = sortByFreq [(d,m) | (d,m) <- rest, m >  n]

> converter 
>   :: [(a, Int)] -> [HTree a]
> converter = map toLeaf
>   where
>     toLeaf :: (a, Int) -> HTree a
>     toLeaf (c,n) = Leaf (c,n)     

  *Ptob50> sortByFreq [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  [('f',5),('e',9),('c',12),('b',13),('d',16),('a',45)]
  *Ptob50> converter it
  [Leaf ('f',5),Leaf ('e',9),Leaf ('c',12),Leaf ('b',13),Leaf ('d',16),Leaf ('a',45)]

  *Ptob50> sortByFreq example
  [('x',1),('u',1),('r',1),('p',1),('o',1)
  ,('t',2),('s',2),('n',2),('m',2),('l',2)
  ,('i',2),('h',2),('f',3),('e',4),('a',4)
  ,(' ',7)
  ]

> weight 
>   :: HTree a -> Int
> weight (Leaf (c,n)) = n
> weight (Node n _ _) = n

> fromList 
>   :: [(a,Int)] -> HTree a
> fromList = fromList' . converter . sortByFreq

> fromList' 
>   :: [HTree a] -- sorted by frequency
>   -> HTree a
> fromList' [l1,l2] = Node (n1+n2) l1 l2
>   where -- They are sorted.
>     n1 = weight l1
>     n2 = weight l2
> fromList' (l1:l2:rest) 
>   = fromList' $ sort' ((fromList' [l1,l2]) : rest) 
>   where
>     sort' :: [HTree a] -> [HTree a]
>     sort' [] = []
>     sort' (l1':rest) = sm ++ (l1':gt)
>       where -- They are already sorted, so just insert our pivot.
>         sm = [l | l <- rest, weight l <= weight l1']
>         gt = [l | l <- rest, weight l >  weight l1']

We have a Huffman tree:
  *Ptob50> fromList [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  Node 100 (Leaf ('a',45)) 
           (Node 55 (Node 25 (Leaf ('c',12)) 
                             (Leaf ('b',13))) 
                    (Node 30 (Node 14 (Leaf ('f',5)) 
                                      (Leaf ('e',9))) 
                             (Leaf ('d',16))))

  *Ptob50> fromList example 
  Node 37 (Node 16 (Node 8 (Leaf ('e',4)) 
                           (Leaf ('a',4))) 
                   (Node 8 (Node 4 (Leaf ('s',2)) 
                                   (Leaf ('n',2))) 
                           (Node 4 (Leaf ('m',2)) 
                                   (Leaf ('l',2))))) 
          (Node 21 (Node 8 (Node 4 (Leaf ('i',2)) 
                                   (Leaf ('h',2))) 
                           (Node 4 (Node 2 (Leaf ('x',1)) 
                                           (Leaf ('u',1))) 
                                   (Node 2 (Leaf ('r',1)) 
                                           (Leaf ('p',1))))) 
                   (Node 13 (Node 6 (Leaf ('f',3)) 
                                    (Node 3 (Leaf ('o',1)) 
                                            (Leaf ('t',2)))) 
                            (Leaf (' ',7))))

Finally we translate the from tree structure to the cipher.

> huffman' 
>   :: HTree a -> [(a,String)]
> huffman' (Leaf (c,_)) 
>   = [(c,"")]
> huffman' (Node _ (Leaf l1@(c1,_)) node2) 
>   = (c1, "0"):(map (helper '1') $ huffman' node2) 
> huffman' (Node _ node1            (Leaf l2@(c2,_)))
>   = (map (helper '0') $ huffman' node1) ++ [(c2,"1")] 
> huffman' (Node _ node1            node2)
>   = (map (helper '0') $ huffman' node1) 
>   ++ (map (helper '1') $ huffman' node2)
>
> helper 
>   :: Char -> (a, String) -> (a, String)
> helper n (c,nums) = (c, n:nums)
>
> huffman 
>   :: (Ord a) => 
>      [(a,Int)] -> [(a,String)]
> huffman = sortByA . huffman' . fromList . sortByFreq 
>
> sortByA  -- alphabetic order
>   :: (Ord a) => 
>      [(a,b)] -> [(a,b)]
> sortByA [] = []
> sortByA (c:cs) = sm ++ (c:gt)
>   where
>     sm = sortByA [d | d <- cs, (fst d) < (fst c)]
>     gt = sortByA [d | d <- cs, (fst d) > (fst c)]
  
  *Ptob50> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
  [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

  *Ptob50> huffman example 
  [(' ',"111"),('a',"000"),('e',"001"),('f',"1100"),('h',"11011")
  ,('i',"0100"),('l',"0101"),('m',"0110"),('n',"0111"),('o',"10100")
  ,('p',"10101"),('r',"10110"),('s',"1000"),('t',"1001"),('u',"10111")
  ,('x',"11010")
  ]

