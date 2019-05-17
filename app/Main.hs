module Main where

import Autopar

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- code to try out the library
main :: IO ()
main = do
    numbers <- map read . lines <$> getContents
    let fibNums = pmap fib numbers
    print (sum fibNums)