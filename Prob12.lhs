Prob12.lhs

> module Prob12 where

> import Prob11 (ListItem(..), modifiedEncode)
> import Test.QuickCheck
 
Decode a run-length encoded list.
Given a run-length code list generated as specified in prob11.lhs.
Construct its uncompressed version.

> toTuple 
>   :: ListItem a -> (Int, a)
> toTuple (Single a)     = (1, a)
> toTuple (Multiple n a) = (n, a)

> toList 
>   :: (Int, a) -> [a]
> toList (1,a) = [a]
> -- toList (n,a) = take n $ cycle [a]
> -- toList (n,a) = take n . repeat $ a
> toList (n,a) = replicate n a
 
> decodeModified 
>   :: [ListItem a] -> [a] 
> -- decodeModified []  = []
> -- decodeModified lst = foldl1 (++) $ map (toList . toTuple) lst
> -- decodeModified lst = foldl (++) [] $ map (toList . toTuple) lst
> -- decodeModified lst = concat $ map (toList . toTuple) lst
> decodeModified = concatMap (toList . toTuple)

Another solution can be implemented by using concatMap:

> decodeConcatMap' 
>   :: [(Int, a)] -> [a]
> decodeConcatMap' = concatMap (uncurry replicate)

  *Main> decodeConcatMap' [(1,'h'),(1,'a'),(1,'s'),(1,'k'),(1,'e'),(2,'l')]
  "haskell"
  
  *Main> decodeConcatMap' $ map toTuple [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] 
  "aaaabccaadeeee"

> decodeConcatMap 
>   :: [ListItem a] -> [a]
> decodeConcatMap = decodeConcatMap' . map toTuple

-- QuickCheck

Let's check the selfconsistency of decode's.

> prop_D_E :: String -> Bool
> prop_D_E xs = (xs == decodeModified (modifiedEncode xs))

  *Prob12> verboseCheckWith stdArgs { maxSuccess = 10000 } prop_D_E
    ...
  Passed:
  "3\242au\DC3\USI;$R\ETB \SI\ENQ\154\180W\235\&0n?\222f]AA>\DC4\234%4)sa(~vJ\188\217\240r\168\230[@\188ocr\r\173*'"
  +++ OK, passed 10000 tests.
