Prob40.lhs

> module Prob40 where

Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. 
Example: 28 = 5 + 23. 
It is one of the most famous conjecture in number theory that has not been proved to be correct in the general case. 
It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). 
Write a predicate to find the two prime numbers that sum up to a given even integer.

  *Prob40> takeWhile (<28) primes
  [2,3,5,7,11,13,17,19,23]
  *Prob40> init $ takeWhile (<28) primes
  [2,3,5,7,11,13,17,19]
  *Prob40> last $ takeWhile (<28) primes
  23

> import Data.Numbers.Primes
> import Prob39 (primesR)

> goldbach, goldbach' 
>   :: Integral a => 
>      a -> (a,a)
> goldbach n 
>   | odd  n = error "input should be an even number"
>   | even n = helper pList n 
>   where
>     pList = takeWhile (<n) primes
>     helper pp@(p:ps) n
>       | biggest + p == n = (p, biggest)
>       | biggest + p >  n = helper newPrimeList n
>       | biggest + p <  n = helper ps n
>       where 
>         biggest = last pp
>         newPrimeList = init pp

> goldbachL 
>   :: Integral t => 
>      t -> [(t, t)]
> goldbachL n = [(x,y) | x<-pr, y<-pr, x<y, x+y == n]
>   where 
>     pr = primesR 2 (n-2)
>
> goldbach' = head . goldbachL

  *Prob40> goldbach' 1000000
  (17,999983)
  (15.05 secs, 5,005,535,400 bytes)
  *Prob40> goldbach 1000000
  (17,999983)
  (14.35 secs, 4,895,268,680 bytes)

