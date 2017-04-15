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

This function is exactly Data.List.group:
  
  > Data.List.group ['a','a','a','a','b','c','c','a','a','d','e','e','e','e']
  ["aaaa","b","cc","aa","d","eeee"]
  > Data.List.group "Mississippi"
  ["M","i","ss","i","ss","i","pp","i"]

GHC's implementation:

> myGroup :: (Eq a) => [a] -> [[a]]
> myGroup = myGroupBy (==)
>
> myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
> myGroupBy _ []     = []
> myGroupBy e (x:xs) = (x:ys) : myGroupBy e zs
>   where
>     (ys,zs) = mySpan (e x) xs
>
> mySpan :: (a -> Bool) -> [a] -> ([a],[a])
> mySpan _ xs@[]     = (xs,xs) 
> mySpan p xx@(x:xs)
>   | p x       = (x:ys, zs)
>   | otherwise = ([],xx)
>   where
>     (ys, zs) = span p xs 

  *Main> myGroup "Mississippi"
  ["M","i","ss","i","ss","i","pp","i"]
