{-# LANGUAGE NumericUnderscores #-}
module Util where

import qualified System.CPUTime                as CPUTime
import           Control.Exception              ( evaluate )
import           Control.DeepSeq                ( force, NFData )
import           Data.Tagged                    ( Tagged(..) )
import qualified Control.Foldl                 as L
import qualified Data.List as List

data Pico
type PicoSeconds = Tagged Pico Integer

-- | The optimal time in picoseconds for a chunk to take.
-- This should be dynamically calculated somehow.
optChunkTime :: PicoSeconds
optChunkTime = 10_000_000_000

minElementTime :: PicoSeconds
minElementTime = 36000000

-- | Evaluate the argument to normal form and measure the execution time.
time :: NFData a => a -> IO PicoSeconds
time x = do
  startTime <- getCPUTime
  _         <- evaluate (force x)
  endTime   <- getCPUTime
  return (endTime - startTime)

-- | Wapper around CPUTime.getCPUTime
getCPUTime :: IO PicoSeconds
getCPUTime = Tagged <$> CPUTime.getCPUTime

-- | Get the average value of a container of numbers.
avg :: (Foldable t, Integral a) => t a -> a
avg = L.fold (div <$> L.sum <*> L.genericLength)

-- | Fibonacci function, just for testing purposes.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib2 :: Int -> Int
fib2 n | n < 2 = n
fib2 n = snd $ List.foldl' f (0,1) [2 .. n]
  where f (pp,p) _ = (p, p+pp)
