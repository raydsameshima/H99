Prob23.lhs

> module Prob23 where

Extract a given number of randomly selected elements from a list.

> import System.Random
> import Control.Monad (replicateM)
> -- replicateM :: Monad m => Int -> m a -> m [a]
> import Data.List (nub)

> rnd_select :: [a] -> Int -> IO [a]
> rnd_select [] _ = return [] 
> rnd_select ls n
>   | n < 0     = error "N must be greater than zero."
>   | otherwise = do 
>       pos <- replicateM n $ 
>                getStdRandom $ randomR (0, (length ls)-1)
>       return [ls !! p | p <- pos]
  
  *Prob23 System.Random Control.Monad> rnd_select [1..100] 1
  [50]
  (0.00 secs, 1,034,720 bytes)
  *Prob23 System.Random Control.Monad> rnd_select [1..100] 1
  [10]
  (0.00 secs, 1,034,736 bytes)
  *Prob23 System.Random Control.Monad> rnd_select [1..100] 1
  [23]

A more elegant solution using
  *Prob23> :t randomR
  randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)

> rnd_select' :: [a] -> Int -> IO [a]
> rnd_select' xs n = do
>   gen <- getStdGen
>   return $ take n [xs !! x | x <- randomRs (0, (length xs)-1) gen]

  *Prob23 System.Random Control.Monad> rnd_select' [1..100] 1
  [95]
  (0.01 secs, 1,549,800 bytes)
  *Prob23 System.Random Control.Monad> rnd_select' [1..100] 1
  [95]
  (0.00 secs, 1,032,432 bytes)
  *Prob23 System.Random Control.Monad> rnd_select' [1..100] 1
  [95]
  (0.00 secs, 1,033,624 bytes) 

Another implementation which uses O(N) algorithm (I'm not sure):

> rnd_select'' :: [a] -> Int -> IO [a]
> rnd_select'' _      0 = return []
> rnd_select'' (x:xs) n = do
>   r <- randomRIO (0, length xs)
>   if r < n 
>     then do
>       rest <- rnd_select'' xs (n-1)
>       return (x : rest)
>     else
>       rnd_select'' xs n

  *Prob23> rnd_select "aiueo" 5
  "uooie"
  *Prob23> rnd_select' "aiueo" 5
  "oaiuo"
  *Prob23> rnd_select'' "aiueo" 5
  "aiueo"

Using aplicative:

> rnd_select''' :: [a] -> Int -> IO [a]
> rnd_select''' lst n = map (lst !!) <$> indices
>   where
>     indices = take n . nub . randomRs (0, (length lst) -1) <$> getStdGen
