Prob24.lhs

> module Prob24 where

Lotto: Draw N different random numbers from the set 1..M.

> import System.Random
> import Prob23 
>   ( randomSelect ) -- [a] -> Int -> IO [a]
> import Data.List 
>   ( nub )

Create a random list of [1..m] which has length n. 

> diffSelect
>   :: Int -> Int -> IO [Int]
> diffSelect n m = randomSelect [1..m] n
