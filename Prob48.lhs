Prob48.lhs

> module Prob48 where

Truth tables for logical expressions (3).

Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. 
Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

  > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
  -- infixl 3 `equ'`
  True  True  True  True
  True  True  False True
  True  False True  True
  True  False False True
  False True  True  True
  False True  False True
  False False True  True
  False False False True
   
  -- infixl 7 `equ'`
  True  True  True  True
  True  True  False True
  True  False True  True
  True  False False False
  False True  True  False
  False True  False False
  False False True  False
  False False False False

> import Prob46
> import Control.Monad (replicateM)

Trial-generalized-boolean function:

> gbf :: [Bool] -> Bool
> gbf _ = True

> tableN :: Int 
>        -> ([Bool] -> Bool) -- generalized boolean function
>        -> IO ()
> tableN n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
>   where
>     args n = replicateM n [True, False]
>     toStr = unwords . map (\x -> show x ++ space x)
>     space True = "  "
>     space False = " "

To replace args n,

> bTable :: Int -> [[Bool]]
> bTable 0 = []
> bTable 1 = [[True], [False]]
> bTable n = map (True:) rest ++ map (False:) rest
>   where
>     rest = bTable (n-1)

> tableN' :: Int 
>        -> ([Bool] -> Bool) -- generalized boolean function
>        -> IO ()
> tableN' n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
>   where
> --    args n = replicateM n [True, False]
>     args = bTable
>     toStr = unwords . map (\x -> show x ++ space x)
>     space True = "  "
>     space False = " "

