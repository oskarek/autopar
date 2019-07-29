module Main where

import Control.Parallel.Strategies
import qualified ParStrategy1 as PS1
import qualified ParStrategy2 as PS2
import System.Environment

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- code to try out the library
main :: IO ()
main = do
    [n] <- getArgs
    numbers <- map read . lines <$> getContents
    let fibNums = map fib numbers `using` strategy n
    print (sum fibNums)

strategy :: NFData a => String -> Strategy [a]
strategy n = case n of
        "0" -> r0
        "1" -> PS1.parList'
        "2" -> PS2.parList'
