Prob33.lhs

> module Prob33 where

Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

> import Prob32 (myGCD)

> coprime :: Integral a => a -> a -> Bool
> coprime n m = (myGCD n m == 1)

  *Prob33> coprime 35 64
  True

