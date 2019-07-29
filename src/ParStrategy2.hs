-- Idea of strategy: Similar to ParStrategy1, but periodically
-- recalculate the chunk size. This to, in theory, be able to better
-- handle irregular lists.
module ParStrategy2
  ( parList'
  )
where

import           Control.Parallel.Strategies
import           System.IO.Unsafe               ( unsafePerformIO )
import           GHC.Conc                       ( getNumCapabilities )
import           Util

parList' :: NFData a => Strategy [a]
parList' xs =
  let nthreads = unsafePerformIO getNumCapabilities
  in  concat <$> parBuffer nthreads rdeepseq (chunk nthreads 0 0 xs)

chunk :: NFData a => Int -> Int -> Int -> [a] -> [[a]]
chunk _        _ _    []       = []
chunk nthreads n size (x : xs) = as : chunk nthreads (n + 1) chunkSize bs
 where
  (as, bs)  = splitAt chunkSize (x : xs)
  chunkSize = if n `mod` nthreads == 0 then calcNewChunkSize x else size

calcNewChunkSize :: NFData a => a -> Int
calcNewChunkSize x =
  let execTime = unsafePerformIO (time x)
  in  fromIntegral (optChunkTime `div` max 1 execTime)
