Prob38.lhs

(*) Compare the two methods of calculating Euler's totient function.

Use the solutions of problems 34 and 37 to compare the algorithms. 
Take the number of reductions as a measure for efficiency. 
Try to calculate phi(10090) as an example.

> import Prob34 (totient'')
> import Prob37 (phi')
>
> main = do
>   putStrLn "Put any integer:"
>   a <- getLine
>   let n = read a :: Integer
>   print $ "totient'' " ++ show n ++ " is " 
>   print $ totient'' n
>   print $ "phi' " ++ show n ++ " is " 
>   print $ phi' n

$ stack runghc Prob38.lhs 
Put any integer:
10090
"totient'' 10090 is "
4032
"phi' 10090 is "
4032

