Prob47.lhs

> module Prob47 where

Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being operators. 
This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). 
Define operator precedence as usual; i.e. as in Java.

> import Prob46

> table2 :: (Bool -> Bool -> Bool) -- Boolean function
>           -> IO ()
> table2 bf 
>   = mapM_ putStrLn 
>     [ show a ++ " " ++ show b ++ " " ++ show (bf a b)
>     | a <- domain, b <- domain]
>     where domain = [True, False]      
