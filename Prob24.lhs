Prob24.lhs

> module Prob24 where

Lotto: Draw N different random numbers from the set 1..M.

> import System.Random
> import Prob23 (rndSelect'')
> import Data.List (nub)

> diffSelect 
>   :: Int -> Int -> IO [Int]
> diffSelect n m 
>   | m > 0     = (`rndSelect''` n) [1 .. m]
>-- | m > 0     = return [1..m] >>= (`rndSelect''` n)
>   | otherwise = return []

Above implementation is the same as the following due to the monad law (left identity (return x >>= f == f x)):

> diff_select' :: Int -> Int -> IO [Int]
> diff_select' n m = rndSelect'' [1..m] n

The following implementation will return different values when called several times.

> diff_select'' :: Int -> Int -> IO [Int]
> diff_select'' n to = ds n [1..to]
>   where
>     ds :: Int -> [Int] -> IO [Int]
>     ds 0 _  = return []
>     ds _ [] = return []
>     ds n xs = do
>       r <- randomRIO (0, length xs -1)
>       let remaining = take r xs ++ drop (r+1) xs
>       rest <- ds (n-1) remaining
>       return ((xs !! r) : rest)

Alternative solution, this much easier to understand:

> diff_select''' :: Int -> Int -> IO [Int]
> diff_select''' n m = do
>   gen <- getStdGen
>   return (take n $ randomRs (1, m) gen)

Note that this does NOT solve the problem, since it does not generate distinct numbers.

Using nub from Data.List, and applicative:

> diff_select'''' :: Int -> Int -> IO [Int]
> diff_select'''' n m = take n . nub . randomRs (1, m) <$> getStdGen

