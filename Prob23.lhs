Prob23.lhs

> module Prob23 where

Extract a given number of randomly selected elements from a list.

> import System.Random
> import Control.Monad 
>   ( replicateM ) -- :: Monad m => Int -> m a -> m [a]
> import Data.List 
>   ( nub ) -- :: Eq a => [a] -> [a]

From the solution:

> rndSelect
>   :: [a] -> Int -> IO [a]
> rndSelect [] _ = return [] 
> rndSelect ls n
>   | n < 0     = error "N must be greater than zero."
>   | otherwise = do 
>     ps <- replicateM n . -- :: m a -> m [a]
>           getStdRandom . -- :: (StdGen -> (a,StdGen)) -> IO a
>           randomR        -- :: RandomGen g => (a, a) -> g -> (a, g)
>           $ (0, (length ls)-1)
>     return [ls !! p | p <- ps]
  
  *Prob23> rndSelect "abcdefgh" 3
  "fhc"
  *Prob23> rndSelect "abcdefgh" 3
  "gah"
  *Prob23> rndSelect "abcdefgh" 3
  "hga"

A more elegant solution using
  randomRs :: (Random a, RandomGen g) => (a, a) -> g -> [a]
We use newStdGen instead of getStdGen.

> rndSelect' 
>   :: [a] -> Int -> IO [a]
> rndSelect' xs n = do
>   gen <- newStdGen
>   let randomIndex = randomRs (0, (length xs) -1) gen
>   return . take n $ [xs !! x | x <- randomIndex]

Another implementation which uses O(N) algorithm (I'm not sure):

> rndSelect'' 
>   :: [a] -> Int -> IO [a]
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

From 
  https://hackage.haskell.org/package/random-1.1/docs/System-Random.html
and for applicative style,
  http://d.hatena.ne.jp/kazu-yamamoto/20101211/1292021817

> rollDice
>   :: IO Int
> rollDice = getStdRandom $ randomR (1, 6)

> rollDices, rollDices' 
>   :: IO [Int]
> -- rollDices = do
> --   g <- newStdGen
> --   return (randomRs (1,6) g)
> rollDices = randomRs (1, 6) <$> newStdGen
>
> rollDices' = randomRs (1,6) <$> newStdGen -- using applicative style
>
> rollDicesN, rollDicesN' 
>   :: Int -> IO [Int]
> rollDicesN n = take n <$> rollDices'
>
> rollDicesN' n = take n . randomRs (1,6) <$> newStdGen

A generalized dice, from 0 to (m-1) for the indeces.

> infDices
>   :: Int -> IO [Int]
> infDices m = randomRs (0,m-1) <$> newStdGen
> gRollDices
>   :: Int -> Int -> IO [Int]
> gRollDices m n = take n <$> infDices m

Using gRollDices to generate quasi-random indeces, randomSelect returns
n random elements of given lst.

> randomSelect
>  :: [a] -> Int -> IO [a]
> randomSelect lst n = do
>   let l = length lst
>   is <- gRollDices l n
>   return [lst !! x | x <- is]
