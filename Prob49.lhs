Prob49.lhs

> module Prob49 where

Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

  > gray 3
  ["000","001","011","010","110","111","101","100"]   
  *Prob48 Control.Monad> replicateM 3 ['0','1']
  ["000","001","010","011","100","101","110","111"]

> import Control.Monad (replicateM)

> gray :: Int -> [String]
> gray n = replicateM n ['0','1']

> gray' :: Int -> [String]
> gray' 0 = [""]
> gray' n = foldr helper [] (gray (n-1))
>   where helper s acc = ("0" ++ s) : ("1" ++ s) : acc

> gray'' :: Int -> [String]
> gray'' 0 = [""]
> gray'' n = [ '0' : x | x <- prev] ++ [ '1' : x | x <- prev]
>   where prev = gray'' (n-1)

  *Prob49> gray 10
  (0.11 secs, 13,586,752 bytes)
  *Prob49> gray' 10
  (0.12 secs, 14,024,096 bytes)
  *Prob49> gray'' 10
  (0.12 secs, 13,997,720 bytes)
