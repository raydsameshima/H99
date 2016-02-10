Prob25.lhs

> module Prob25 where

Generate a random permutation of the elements of a list.

> import System.Random
> import Data.List (nub)

Using the same method of Prob23:

> rnd_permu :: [a] -> IO [a]
> rnd_permu lst = map (lst !!) <$> randomIndices (length lst)

> randomIndices :: Int -> IO [Int]
> randomIndices n = take n . nub . randomRs (0, n-1) <$> getStdGen

We can generate the permutation recursively:

> rnd_permu' :: [a] -> IO [a]
> rnd_permu' [] = return []
> rnd_permu' (x:xs) = do
>   rand <- randomRIO (0, (length xs))
>   rest <- rnd_permu xs
>   return $ let (ys,zs) = splitAt rand rest
>            in  ys ++ (x : zs)

  *Prob25> splitAt 3 [0..9]
  ([0,1,2],[3,4,5,6,7,8,9])
