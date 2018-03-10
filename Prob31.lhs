Prob31.lhs

> module Prob31 where

Determine whether a given integer number is prime.

See for the implementation:
  http://qiita.com/bra_cat_ket/items/6a99a9b01682886607d0

Order preserving union for two lists.

> merge 
>   :: (Ord a) => 
>      [a] -> [a] -> [a]
> merge xx@(x:xs) yy@(y:ys)
>   | x <  y = x : (merge xs yy)
>   | x == y = x : (merge xs ys)
>   | x >  y = y : (merge xx ys)

Difference lists, also order preserving.

> diff 
>   :: (Ord a) => 
>      [a] -> [a] -> [a]
> diff xx@(x:xs) yy@(y:ys)
>   | x <  y = x : (diff xs yy)
>   | x == y = diff xs ys
>   | x >  y = diff xx ys

Let us make the list of nonPrimes, then taking difference from odds, 
we have the list of primes:

> primes, nonPrimes 
>   :: (Integral a) => 
>      [a]
> primes = 2:3:5: (diff [7,9..] nonPrimes)
> nonPrimes = foldr1 merge' $ map helper $ tail primes
>   where
>     merge' 
>       :: (Ord a) => 
>          [a] -> [a] -> [a]
>     merge' (x:xs) ys = x : (merge xs ys)
>
>     helper 
>       :: (Integral a) => 
>          a -> [a]
>     helper p = [n*p | n <- [p, p+2..]]

  *Prob31> 7917 `elem` takeWhile (<7919) primes
  False

> isPrime 
>   :: (Integral a) => 
>      a -> Bool
> isPrime n
>   | odd n = all ((/= 0) . mod n) $ takeWhile (<= upperLimit) primes
>   | otherwise = False
>   where 
>     upperLimit = floor . sqrt $ fromIntegral n

Of course, we don't have to make primes to check whether each input is 
prime or not!
So this is clearly a super inefficient implementation.

The below implementation is from 
  https://wiki.haskell.org/99_questions/Solutions/31
All primes larger than 2,3 can be written as 
  6*m + 1, 6*m + 5
forms.
In addition, all we have to do is to check whether the input n has its 
factors up to sqrt(n):

> isPrime' 
>   :: (Integral a) => 
>      a -> Bool
> isPrime' n
>   | n < 2            = False
>   | n == 2 && n == 3 = True
> isPrime' n = all ((/=0) . mod n) $ takeWhile (<= upperLimit) candidates
>   where
>     candidates 
>       :: (Integral int) => 
>          [int]
>     candidates = 2:3: [x+i | x <- [6,12..], i <- [-1,1]]
>
>     upperLimit 
>       :: (Integral int) => 
>          int
>     upperLimit = floor . sqrt $ fromIntegral n

The following is, on the other hand, super slow.

> primes' 
>   :: (Integral a) => 
>      [a]
> primes' = 2 : [n| n<-[3,5..], isPrime' n] 

  *Prob31> isPrime 982451653
  True
  (0.19 secs, 38,213,232 bytes)
  *Prob31> isPrime' 982451653
  True
  (0.03 secs, 6,751,536 bytes)

