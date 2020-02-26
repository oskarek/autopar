module Main where

import           Control.Parallel.Strategies
import qualified ParStrategy1                  as PS1
import qualified ParStrategy2                  as PS2
import qualified ParStrategy3                  as PS3
import qualified ParStrategy4                  as PS4
import           System.Environment
import qualified Data.Map                      as Map
import           Data.Map                       ( (!) )
import           Util                           
import Data.List.Split
import Autopar
import ChunkParList
import Data.Monoid

-- code to try out the library
main :: IO ()
main = do
  [n]     <- getArgs
  -- numbers <- map read . lines <$> getContents
  --let fibNums = map fib nums `using` strategies ! n
  -- print (evalList' (chunkParBuffer 2168) nums2)
  -- print (pFoldMap (Sum . fib) nums)
  -- chunkedParBuffer
  -- sum_in_threads_2
  -- myVersion
  return ()

evalList' :: Strategy [Int] -> [[Int]] -> Int
evalList' strat lst = sum (map (sum . map (+1)) lst `using` strat)

nums :: [Int]
nums = replicate 100000 9

nums2 :: [[Int]]
nums2 = replicate 100000 [1 .. 275]

-- myParBuffer :: NFData a => Strategy [a]
-- myParBuffer xs = do
--   let l = chunk' 5000 xs
--   concat <$> parBuffer 16 rdeepseq l

stratTest :: NFData a => Strategy [a]
stratTest xs = concat <$> parList rdeepseq (chunksOf 62500 xs)

strategies :: NFData a => Map.Map String (Strategy [a])
strategies = Map.fromList
  [ ("0", r0)
  , ("00", parBuffer 16 rdeepseq)
  , ("000", stratTest)
  , ("0000", parListChunk 3333 rdeepseq)
  -- , ("00000", chunkParBuffer 5000)
  , ("1", PS1.parList)
  , ("2", PS2.parList)
  , ("3", PS3.parList)
  , ("4", PS4.parList)
  ]


-- 960_000 elements, 48 chunks, 20_000 element per chunk

normalParBuffer, sum_in_threads, sum_in_threads_2, myVersion, seqential :: IO ()
normalParBuffer = print $ sum (map fib2 (take 960000 $ cycle [180,181]) `using` parBuffer 16 rdeepseq)

sum_in_threads = print $ sum (map (f 5000 0) (take 192 $ cycle [180,181]) `using` parBuffer 16 rdeepseq)
  where f 0 s _ = s
        f n s x = f (n-1) (s + fib2 x) x

sum_in_threads_2 = print $ psum $ map fib2 $ take 960000 (cycle [180,181])

myVersion = print $ sum (map fib2 (take 960000 $ cycle [180,181]) `using` PS1.parList)

seqential = print $ sum (map fib2 (take 960000 $ cycle [180,181]))

-- chunkedParBuffer = print $ sum (map fib2 (take 960000 $ cycle [180,181]) `using` myParBuffer)