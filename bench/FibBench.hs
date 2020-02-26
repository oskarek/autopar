{-# LANGUAGE NumericUnderscores, BangPatterns #-}
module FibBench
  ( benchmarks
  )
where

import           Control.Parallel.Strategies
                                         hiding ( evalList )
import           Util
import           Criterion
import qualified Data.Map                      as M
import           Common

-- | Set which fib numbers should be used for each list length.
fibs :: M.Map Int [[Int]]
fibs = M.mapWithKey (\l xs -> replicate l <$> xs) $ M.fromList
  [ (10       , [12])
  , (100      , [])
  , (1_000    , [])
  , (10_000   , [12])
  , (100_000  , [12])
  , (1_000_000, [12])
  ]


fibTime :: M.Map Int Int
fibTime = M.fromAscList ((\x -> (x, round $ 4000 * 1.6 ^ x)) <$> [0 .. 40])

strategies :: NFData a => Int -> [NamedStrategy [a]]
strategies fib = (namedStrat <$> chunkTimes) ++ [NamedStrategy "Sequential" r0]
 where
  title size t = concat
    [ "chunk time&size: "
    , show (fromIntegral t / 10 ^ 9)
    , "ms&" ++ show size ++ "elems"
    ]
  namedStrat t =
    let size = max 1 $ fromIntegral t `div` fibTime M.! fib
    in  NamedStrategy (title size t) (chunkParBuffer size)

mkBench :: [Int] -> NamedStrategy [Int] -> Benchmark
mkBench xs !st = bench (name st) (whnf (evalList $ strat st) xs)

benchmarks :: [Benchmark]
benchmarks = listLengthBgroup <$> listLengths
 where
  listLengthBgroup n =
    bgroup ("list length " ++ show n) (fibBgroup <$> fibs M.! n)
  fibBgroup xs =
    bgroup ("fib " ++ show (head xs)) (mkBench xs <$> strategies (head xs))

-- | Map fib function over the given list in parallel, using
-- the given strategy.
evalList :: Strategy [Int] -> [Int] -> Int
evalList strat lst = sum (map fib lst `using` strat)
