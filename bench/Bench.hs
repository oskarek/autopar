{-# LANGUAGE NumericUnderscores #-}
module Main where

import           Criterion.Main
import qualified FibBench
import qualified ListLengthBench
import qualified ListMapBench

benchss :: [Benchmark]
benchss = [ bgroup "lengths" ((\n -> bench (show n) (nf length [1 .. n])) <$> [ 10, 20 .. 600 ])
          , bgroup "maps" ((\n -> bench (show n) (nf (map (+1)) [1 .. n])) <$> [ 1 .. 70 :: Int ]) ]

main :: IO ()
main = defaultMain ListLengthBench.benchmarks
