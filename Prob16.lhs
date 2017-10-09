Prob16.lhs

> module Prob16 where

Drop every N'th element from a list.

> generator 
>   :: Int -> [Int]
> generator n = concat $ repeat [1..n]

  *Prob16> zip (generator 3) "abcdefghik"
  [(1,'a'),(2,'b'),(3,'c'),(1,'d'),(2,'e'),(3,'f'),(1,'g'),(2,'h'),(3,'i'),(1,'k')]

> isNotNth 
>   :: Int -> (Int,a) -> Bool
> -- isNotNth n (m,_) = not (n == m)
> isNotNth n (m,_) = n /= m

  *Prob16> zip (generator 3) "abcdefghik"
  [(1,'a'),(2,'b'),(3,'c'),(1,'d'),(2,'e'),(3,'f'),(1,'g'),(2,'h'),(3,'i'),(1,'k')]
  *Prob16> filter (isNotNth  3) it
  [(1,'a'),(2,'b'),(1,'d'),(2,'e'),(1,'g'),(2,'h'),(1,'k')]
  *Prob16> map snd it
  "abdeghk"
  *Prob16> map snd $ filter (isNotNth 3) $ zip (generator 3) "abcdefghijk"
  "abdeghjk"

> dropEvery 
>   :: [a] -> Int -> [a]
> dropEvery lst n = map snd $ filter (isNotNth n) $ zip (generator n) lst

In my implementation,
  generator n
is the same as the follwing Prelude function:
  cycle [1..n]
In addition
  isNotNth n
can be written as
  (n /=) . fst
and then we have the following:

> dropEvery' 
>   :: [a] -> Int -> [a]
> -- dropEvery' lst n = map snd $ filter (\x -> n /= fst x) $ zip (cycle [1..n]) lst
> dropEvery' lst n = map snd $ filter ((n /=) . fst) $ zip (cycle [1..n]) lst

Using zip and list comprehensions

> dropEvery'' 
>   :: [a] -> Int -> [a]
> dropEvery'' xs n = [i| (c,i) <- (zip [1..] xs), (c `mod` n) /= 0]
