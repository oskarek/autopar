-- Idea of strategy: Have a predetermined 'optimal chunk time'
-- calculated, that says how long a chunk should optimally take
-- to evaluate. (valid approach? how to get this number?)
-- Then, when evaluating the list, calculate the time it takes to
-- evaluate the first element in the list and base the chunk size on that.
module ParStrategy1
  ( parList'
  )
where

import           Control.Parallel.Strategies
import           System.IO.Unsafe               ( unsafePerformIO )
import qualified Data.List.Split               as Split
import           GHC.Conc                       ( getNumCapabilities )
import           Util

parList' :: NFData a => Strategy [a]
parList' xs =
  let nthreads = unsafePerformIO getNumCapabilities
  in  concat <$> parBuffer nthreads rdeepseq (chunk xs)

chunk :: NFData a => [a] -> [[a]]
chunk [] = []
chunk (x : xs) =
  let execTime  = unsafePerformIO (time x)
      chunkSize = fromIntegral (optChunkTime `div` max 1 execTime)
  in  Split.chunksOf chunkSize (x : xs)
