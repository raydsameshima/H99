Prob12.lhs

> module Prob12 where

> import Prob11
> import Test.QuickCheck
 
Decode a run-length encoded list.
Given a run-length code list generated as specified in prob11.lhs.
Construct its uncompressed version.

> -- data ListItem a = Multiple Int a 
> --                 | Single a
> --                 deriving (Show)

> toTuple :: ListItem a -> (Int, a)
> toTuple (Single a)     = (1, a)
> toTuple (Multiple n a) = (n, a)

> toList :: (Int, a) -> [a]
> toList (1,a) = [a]
> toList (n,a) = take n (cycle [a])
 
> decodeModified :: [ListItem a] -> [a] 
> decodeModified []  = []
> decodeModified lst = foldl1 (++) $ map (toList . toTuple) lst

Another solution can be implemented by using concatMap:

> decodeConcatMap' :: [(Int, a)] -> [a]
> decodeConcatMap' = concatMap (uncurry replicate) 

  *Main> decodeConcatMap' [(1,'h'),(1,'a'),(1,'s'),(1,'k'),(1,'e'),(2,'l')]
  "haskell"
  
  *Main> decodeConcatMap' $ map toTuple [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] 
  "aaaabccaadeeee"

> decodeConcatMap :: [ListItem a] -> [a]
> decodeConcatMap = decodeConcatMap' . map toTuple

Let's check the selfconsistency of decode's.

> prop_D_E xs = (xs == decodeModified (modifiedEncode xs))
> -- prop_E_D xs = (xs == modifiedEncode (decodeModified xs))

  *Prob12> quickCheck prop_D_E 
  +++ OK, passed 100 tests.
