Prob04.lhs

> module Prob04 where

Find the number of elements of a list.

> myLength :: [a] -> Int
> myLength lst = myLength' lst 0 
>   where 
>     myLength' []     n = n
>     myLength' (_:xs) n = myLength' xs (n+1)

This n is so-called the accumulator.

> myFoldlLength :: [a] -> Int
> myFoldlLength = foldl (\n _ -> n+1) 0

  *Main> zip [1..] "aiueo"
  [(1,'a'),(2,'i'),(3,'u'),(4,'e'),(5,'o')]
  *Main> fst . last $ it
  5

> myZipLength :: [b] -> Int
> myZipLength = fst . last . zip [1..] 

> my1Length :: [a] -> Int
> my1Length = sum . map (\_ -> 1)

> my1Length' = sum . map (const 1)
