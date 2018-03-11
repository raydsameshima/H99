Prob32.lhs

> module Prob32 where

Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
https://en.wikipedia.org/wiki/Euclidean_algorithm

The following implementation is the subtraction-based version which was Euclid's original version.

> myGCD 
>   :: Integral a => 
>      a -> a -> a
> myGCD a b
>   | b < 0 = myGCD a (-b)
> myGCD a b
>  | a == b = a
>  | b >  a = myGCD b a
>  | b <  a = myGCD (a-b) b 

  *Prob32> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
  [9,3,3]
  *Prob32> myGCD 1071 462
  21
