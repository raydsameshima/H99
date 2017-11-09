Prob23.lhs

> module Prob23 where

Extract a given number of randomly selected elements from a list.

> import System.Random
> import Control.Monad (replicateM) -- :: Monad m => Int -> m a -> m [a]
> import Data.List (nub)

From the solution:

> rndSelect
>   :: [a] -> Int -> IO [a]
> rndSelect [] _ = return [] 
> rndSelect ls n
>   | n < 0     = error "N must be greater than zero."
>   | otherwise = do 
>     pos <- replicateM n . -- :: m a -> m [a]
>            getStdRandom . -- :: (StdGen -> (a,StdGen)) -> IO a
>            randomR 
>            $ (0, (length ls)-1)
> {-
>     pos <- replicateM n $ -- :: Monad m => Int -> m a -> m [a]
>            getStdRandom $ -- :: (StdGen -> (a,StdGen)) -> IO a
>            randomR -- :: (Random a,RandomGen g) => (a,a) -> g -> (a,g)
>            (0, (length ls)-1)
> -}
>     return [ls !! p | p <- pos]
  
  *Prob23> rndSelect "abcdefgh" 3
  "fhc"
  *Prob23> rndSelect "abcdefgh" 3
  "gah"
  *Prob23> rndSelect "abcdefgh" 3
  "hga"

A more elegant solution using
  *Prob23> :t randomR
  randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)

> rndSelect' 
>   :: [a] -> Int -> IO [a]
> rndSelect' xs n = do
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

> rndSelect'' :: [a] -> Int -> IO [a]
> rndSelect'' _      0 = return []
> rndSelect'' (x:xs) n = do
>   r <- randomRIO (0, length xs)
>   if r < n 
>     then do
>       rest <- rndSelect'' xs (n-1)
>       return (x : rest)
>     else
>       rndSelect'' xs n

  *Prob23> rnd_select "aiueo" 5
  "uooie"
  *Prob23> rnd_select' "aiueo" 5
  "oaiuo"
  *Prob23> rnd_select'' "aiueo" 5
  "aiueo"

Using aplicative:

> rndSelect''' :: [a] -> Int -> IO [a]
> rndSelect''' lst n = map (lst !!) <$> indices
>   where
>     indices = take n . nub . randomRs (0, (length lst) -1) <$> getStdGen

From an example on hackage,
  https://hackage.haskell.org/package/random-1.1/docs/System-Random.html
 
> rollDice :: IO Int
> rollDice = getStdRandom . randomR $ (1,6)

  https://qiita.com/philopon/items/8f647fc8dafe66b7381b

> rndSelect4 :: [a] -> Int -> IO [a]
> rndSelect4 = undefined
> {-
> rndSelect4 lst n = do
>   is <- getStdRandom . randomRs $ (1, length lst)
>   return $ take n $ map (lst !!) is
> -}
