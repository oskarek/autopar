module Main where

import           Control.Parallel.Strategies
import qualified ParStrategy1                  as PS1
import qualified ParStrategy2                  as PS2
import qualified ParStrategy3                  as PS3
import qualified ParStrategy4                  as PS4
import           System.Environment
import qualified Data.Map                      as Map
import           Data.Map                       ( (!) )

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- code to try out the library
main :: IO ()
main = do
  [n]     <- getArgs
  numbers <- map read . lines <$> getContents
  let fibNums = map fib numbers `using` strategies ! n
  print (sum fibNums)

strategies :: NFData a => Map.Map String (Strategy [a])
strategies = Map.fromList
  [ ("0", r0)
  , ("1", PS1.parList)
  , ("2", PS2.parList)
  , ("3", PS3.parList)
  , ("4", PS4.parList)
  ]
