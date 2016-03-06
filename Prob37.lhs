Prob37.lhs

> module Prob37 where
> import Prob34 (totient)
> import Prob36 (primeFactorsMult)

Calculate Euler's totient function phi(m) (improved).

See problem 34 for the definition of Euler's totient function. 
If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: 
Let 
  [(p1, m1), (p2, m2), (p3, m3)..]
be the list of prime factors (and their multiplicities) of a given number m. 
Then phi(m) can be calculated with the following formula:
  (p1-1)*p1^(m1-1) * (p2-1)*p2^(m2-1) * ..
  
  *Prob37> primeFactorsMult 315
  [(7,1),(5,1),(3,2)]
  *Prob37> map (\(p,m) -> (p-1)*p^(m-1)) it
  [6,4,6]
  *Prob37> product it
  144

> phi :: Integral a => a -> a
> phi n = product $ map (\(p,m) -> (p-1)*p^(m-1)) $ primeFactorsMult n

  *Prob31> isPrime 12073
  True

  *Prob37> phi 12073 
  12072
  (0.06 secs, 19,111,352 bytes)
  *Prob37> totient 12073 
  12072
  (2.18 secs, 604,187,792 bytes)

Using list comprehension, here is a beautiful solution:

> phi' :: Integral a => a -> a
> phi' n = product [(p-1)*p^(m-1) | (p,m) <- primeFactorsMult n]

  *Prob37> totient 10090
  4032
  (1.59 secs, 438,197,056 bytes)
  *Prob37> phi 10090
  4032
  (0.01 secs, 4,160,272 bytes)
  *Prob37> phi' 10090
  4032
  (0.01 secs, 2,613,072 bytes)
