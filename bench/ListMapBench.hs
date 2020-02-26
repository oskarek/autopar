{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores, BangPatterns #-}
module ListMapBench where

import           Control.Parallel.Strategies
                                         hiding ( evalList )
import           Criterion
import qualified Data.Map                      as M
import           Common

lengths :: M.Map Int [(Int, Int)]
lengths =
  M.mapWithKey (\l -> fmap (l,)) $ M.fromList
    [ (10       , [17, 52, 150, 380])
    , (100      , [])
    , (1_000    , [])
    , (10_000   , [17, 52, 150, 380])
    , (100_000  , [17, 52, 150, 380])
    , (1_000_000, [17, 52, 150, 380])
    ]

traverseTime :: M.Map Int Int
traverseTime =
  M.fromAscList [(17, 67109), (52, 171799), (150, 439805), (380, 1125900), (275, 4611686), (965, 18889466)]

strategies :: NFData a => Int -> [NamedStrategy [a]]
strategies len = (namedStrat <$> chunkTimes) ++ [NamedStrategy "Sequential" r0]
 where
  title size t = concat
    [ "chunk time&size: "
    , show (fromIntegral t / 10 ^ 9)
    , "ms&" ++ show size ++ "elems"
    ]
  namedStrat t =
    let size = max 1 $ fromIntegral t `div` traverseTime M.! len
    in  NamedStrategy (title size t) (chunkParBuffer size)

mkBench :: (Int, Int) -> NamedStrategy [Int] -> Benchmark
mkBench (n, nchunk) !st = env (return $ replicate n [1 .. nchunk]) createBench
    where createBench xs = bench (name st) (whnf (evalList $ strat st) xs)

benchmarks :: [Benchmark]
benchmarks = listLengthBgroup <$> listLengths
 where
  listLengthBgroup n =
    bgroup ("list length " ++ show n) (fibBgroup <$> lengths M.! n)
  fibBgroup (n,nchunk) =
    bgroup ("length " ++ show nchunk) (mkBench (n,nchunk) <$> strategies nchunk)

evalList :: Strategy [Int] -> [[Int]] -> Int
evalList strat lst = sum (map (sum . map (+1)) lst `using` strat)
