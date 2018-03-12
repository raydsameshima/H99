Prob48.lhs

> module Prob48 where

Truth tables for logical expressions (3).

Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. 
Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.
  
  *Prob48> tableN 3 (\[a,b,c] -> (a && (b || c)) == (a && b) || (a && c))
  True   True   True   => True
  True   True   False  => True
  True   False  True   => True
  True   False  False  => True
  False  True   True   => True
  False  True   False  => True
  False  False  True   => True
  False  False  False  => True

> -- import Prob46
> import Control.Monad (replicateM)

A trial-generalized-boolean function:

> gbf 
>   :: [Bool] -> Bool
> gbf _ = True

> tableN 
>   :: Int 
>   -> ([Bool] -> Bool) -- generalized boolean function
>   -> IO ()
> tableN n f =
>   mapM_ putStrLn [ toStr a ++ " => " ++ show (f a) 
>                  | a <- args n
>                  ]
>     where
>       args :: Int -> [[Bool]]
>       args n = replicateM n [True, False]
>
>       toStr :: [Bool] -> String
>       toStr = unwords . map (\x -> show x ++ space x)
>
>       space :: Bool -> String
>       space True  = "  "
>       space False = " "

  *Prob48> tableN 3 gbf 
  True   True   True   => True
  True   True   False  => True
  True   False  True   => True
  True   False  False  => True
  False  True   True   => True
  False  True   False  => True
  False  False  True   => True
  False  False  False  => True

To replace args n, without using replicateM:

> bTable 
>   :: Int -> [[Bool]]
> bTable 0 = []
> bTable 1 = [[True], [False]]
> bTable n = map (True:) rest ++ map (False:) rest
>   where
>     rest = bTable (n-1)

> tableN' 
>   :: Int 
>   -> ([Bool] -> Bool) -- generalized boolean function
>   -> IO ()
> tableN' n f = 
>   mapM_ putStrLn [ toStr a ++ " => " ++ show (f a) 
>                  | a <- bTable n
>                  ]
>     where
>       toStr :: [Bool] -> String
>       toStr = unwords . map (\x -> show x ++ space x)
>
>       space :: Bool -> String
>       space True  = "  "
>       space False = " "

  *Prob48> tableN 3 (\[a,b,c] -> (a && (b || c)) == (a && b) || (a && c))
  True   True   True   => True
  True   True   False  => True
  True   False  True   => True
  True   False  False  => True
  False  True   True   => True
  False  True   False  => True
  False  False  True   => True
  False  False  False  => True
  *Prob48> tableN' 3 (\[a,b,c] -> (a && (b || c)) == (a && b) || (a && c))
  True   True   True   => True
  True   True   False  => True
  True   False  True   => True
  True   False  False  => True
  False  True   True   => True
  False  True   False  => True
  False  False  True   => True
  False  False  False  => True
