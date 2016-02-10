prob03.lhs

Find the K'th element of a list.
The first element is the list is number 1.

> elementAt :: [a] -> Int -> a
> elementAt (x:_)  1 = x
> elementAt (_:xs) n = elementAt xs (n-1)
> elementAt _      _ = error "Index out of bounds"

Implicitly assumed the number is less than the length of the list!

> elementAt' [] _ = error "Index out of bounds"
> elementAt' (_:xs) n
>   | n <= 0    = error "Index out of bounds"
>   | otherwise = elementAt' xs (n-1)

Using an infix operator !!,

> elementAt'' :: [a] -> Int -> a
> elementAt'' lst n = lst !! (n-1)

Let's take a moment,

  *Main> zip [1..] "haskell"
  [(1,'h'),(2,'a'),(3,'s'),(4,'k'),(5,'e'),(6,'l'),(7,'l')]
  *Main> filter (\(n,c) -> n == 5) it
  [(5,'e')]
  *Main> snd . head $ it
  'e'

> elementAt''' lst n = snd . head $ filter (\(m,_) -> m == n) $ zip [1..] lst
