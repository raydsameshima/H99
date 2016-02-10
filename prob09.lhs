prob09.lhs

Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

> pack :: Eq a => [a] -> [[a]]
> pack [] = []
> pack (x:xs) = (x:front) : pack rear
>   where front = takeWhile (== x) xs
>         rear  = dropWhile (== x) xs   

Slightly modifed version of the solution.

  takeWhile :: (a -> Bool) -> [a] -> [a]
  takeWhile _ [] =  []
  takeWhile p (x:xs)
    | p x        = x : takeWhile p xs
    | otherwise  = []

  dropWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile _ [] =  []
  dropWhile p xs@(x:xs')
    | p x        = dropWhile p xs'
    | otherwise  = xs

