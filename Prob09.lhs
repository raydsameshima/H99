Prob09.lhs

> module Prob09 where
> import Data.List (group)

Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

I.e., make your Data.List.group.

> pack 
>   :: (Eq a) => 
>      [a] -> [[a]]
> pack []     = []
> pack (x:xs) = (x:front) : pack rear
>   where 
>     front = takeWhile (== x) xs
>     rear  = dropWhile (== x) xs   

Slightly modified version of the solution.

  takeWhile :: (a -> Bool) -> [a] -> [a]
  takeWhile _ []     =  []
  takeWhile p (x:xs)
    | p x            = x : takeWhile p xs
    | otherwise      = []

  dropWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile _ []         = []
  dropWhile p xs@(x:xs')
    | p x                = dropWhile p xs'
    | otherwise          = xs

This function is exactly Data.List.group:
  
  > group ['a','a','a','a','b','c','c','a','a','d','e','e','e','e']
  ["aaaa","b","cc","aa","d","eeee"]
  > group "Mississippi"
  ["M","i","ss","i","ss","i","pp","i"]

GHC's implementation:

> myGroup 
>   :: (Eq a) => 
>      [a] -> [[a]]
> myGroup = myGroupBy (==)
>
> myGroupBy 
>   :: (a -> a -> Bool) -> [a] -> [[a]]
> myGroupBy _ []     = []
> myGroupBy e (x:xs) = (x:ys) : myGroupBy e zs
>   where
>     (ys,zs) = mySpan (e x) xs
>
> mySpan 
>   :: (a -> Bool) -> [a] -> ([a],[a])
> mySpan _ xs@[]     = (xs,xs) 
> mySpan p xx@(x:xs)
>   | p x       = (x:ys, zs)
>   | otherwise = ([],xx)
>   where
>     (ys, zs) = span p xs 

  *Main> myGroup "Mississippi"
  ["M","i","ss","i","ss","i","pp","i"]

> packIf 
>   :: Eq a => 
>      [a] -> [[a]]
> packIf []     = []
> packIf [x]    = [[x]]
> packIf (x:xs) = if x `elem` headPackXs
>                   then (x:headPackXs) : (tail packXs)
>                   else [x] : packXs
>   where headPackXs = head packXs
>         packXs = pack xs
> {-
> packIf (x:xs) = if x `elem` (head (pack xs))
>                   then (x:(head (pack xs))) : (tail (pack xs))
>                   else [x] : (pack xs)
> -}

> packF :: (Eq a) => [a] -> [[a]]
> packF = foldr f []
>   where
>     f :: (Eq a) => a -> [[a]] -> [[a]]
>     f x [] = [[x]]
>     f x (ys:yss) 
>            = if x == head ys
>                then (x:ys):yss
>                else [x] : ys : yss

