Prob34.lhs

> module Prob34 where

Calculate Euler's totient function phi(m).
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

> import Prob33 (coprime)

  *Prob33> length $ filter (coprime 10) [1..10]
  4

> totient :: Integral a => a -> Int
> totient m 
>   | m > 0 = length $ filter (coprime m) [1..m]

  *Prob34> map totient [1..20]
  [1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8]

  https://oeis.org/A000010
