Prob49.lhs

> module Prob49 where

Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

  > gray 3
  ["000","001","011","010","110","111","101","100"]   

  *Prob49> replicateM 3 ['0','1']
  ["000","001","010","011","100","101","110","111"]

> import Control.Monad (replicateM)

> gray 
>   :: Int -> [String]
> gray n = replicateM n ['0','1']

> gray' 
>   :: Int -> [String]
> gray' 0 = [""]
> gray' n = foldr helper [] (gray (n-1))
>   where 
>     helper s acc = ("0" ++ s) : ("1" ++ s) : acc

> gray'' 
>   :: Int -> [String]
> gray'' 0 = [""]
> gray'' n = [ '0' : x | x <- prev] ++ [ '1' : x | x <- reverse prev]
>   where 
>     prev = gray'' (n-1)

So far above implementations return well-formed strings.
An efficient way with foldr from the solution:

> grayr 
>   :: Integral i =>
>      i -> [String]
> grayr 0 = [""]
> grayr n = foldr helper [] (grayr (n-1))
>   where
>     helper s acc = ("0" ++ s) : ("1" ++ s) : acc

That is, we add 0 and 1 as heads.

  *Prob49> grayr 3
  ["000","100","010","110","001","101","011","111"]
  *Prob49> grayr 4
  ["0000","1000","0100","1100","0010","1010","0110","1110"
  ,"0001","1001","0101","1101","0011","1011","0111","1111"]

  *Prob49> length $ gray 23
  8388608
  (4.19 secs, 1,476,486,832 bytes)
  *Prob49> length $ grayr 23
  8388608
  (2.27 secs, 1,744,928,600 bytes)
  *Prob49> length $ gray'' 23
  8388608
  (4.20 secs, 2,550,232,496 bytes)

