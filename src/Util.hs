{-# LANGUAGE NumericUnderscores #-}
module Util where

import           Control.Parallel.Strategies
import qualified System.CPUTime                as CPUTime
import           Control.Exception              ( evaluate )
import           Control.DeepSeq                ( force )
import           Data.Tagged

data Pico
type PicoSeconds = Tagged Pico Integer

-- | The optimal time in picoseconds for a chunk to take.
-- This should be dynamically calculated somehow.
optChunkTime :: PicoSeconds
optChunkTime = 10_000_000_000

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
