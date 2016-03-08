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
