Prob34.lhs

> module Prob34 where

Calculate Euler's totient function phi(m).
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

> import Data.List (nub)
> import Data.Ratio
> import Data.Numbers.Primes
>
> import Prob33 (coprime)

  *Prob33> length $ filter (coprime 10) [1..10]
  4

> totient, totient'
>   :: Integral a => 
>      a -> Int
> totient m 
>   | m > 0 = length $ filter (coprime m) [1..m]
>   | otherwise = error "totient: "

  *Prob34> map totient [1..20]
  [1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8]

  https://oeis.org/A000010

Using list comprehension:

> totient' m = length [x | x<-[1..m], coprime x m]

See wikipedia,
  https://en.wikipedia.org/wiki/Euler%27s_totient_function#Computing_Euler's_totient_function

> totient''
>   :: (Integral int) =>
>      int -> int
> totient'' 1 = 1
> totient'' n = numerator r `div` denominator r
>   where
>     ps = nub $ primeFactors n
>     rs = map (\x -> (1-(1%x))) ps
>     r  = (n%1) * product rs
>     
> --  r = foldl (\acc x -> acc * (1 - (1%x))) (n%1) ps
> 
