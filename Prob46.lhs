Prob46.lhs

> module Prob46 where

Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

  > table (\a b -> (and' a (or' a b)))
  True True True
  True False True
  False True False
  False False False

> and', or' :: Bool -> Bool -> Bool
> and' True  True  = True
> and' _     _     = False
> or'  False False = False
> or'  _     _     = True

> not' :: Bool -> Bool
> not' True  = False
> not' False = True
>
> nand', nor' :: Bool -> Bool -> Bool
> nand' a b = not' (and' a b)
> nor'  a b = not' (or' a b)

> xor' :: Bool -> Bool -> Bool
> xor' a b = (a /= b)

> -- (==>)
> impl :: Bool -> Bool -> Bool
> True  `impl` b = b
> False `impl` _ = True

> equ :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
> equ bf1 bf2 = foldl and' True [bf1 x == bf2 x| x <- [True, False]] 

  *Prob46> :t print 
  print :: Show a => a -> IO ()
  *Prob46> print True
  True
  *Prob46> :t show
  show :: Show a => a -> String
  *Prob46> show True ++ " " ++ show False
  "True False"
  *Prob46> putStrLn $ show True ++ " " ++ show False
  True False

