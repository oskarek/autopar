module ChunkParList where

import           Control.Parallel.Strategies
import           System.IO.Unsafe               ( unsafePerformIO )
import           GHC.Conc                       ( getNumCapabilities )
import           Util
import           Chunker

-- | Create a lazy list evaluation strategy, that evaluates 
-- the list in chunks, according to the given Chunker.
chunkParList :: NFData a => Chunker a -> Strategy [a]
chunkParList = chunkParList' optChunkTime

chunkParList' :: NFData a => PicoSeconds -> Chunker a -> Strategy [a]
chunkParList' opt chunker xs =
  let nthreads = unsafePerformIO getNumCapabilities
      chunks   = evalChunker chunker opt xs
  in  concat <$> parBuffer (2*nthreads) rdeepseq chunks

chooseBasedOnFirst :: NFData a => Strategy [a] -> Strategy [a]
chooseBasedOnFirst _ [] = return []
chooseBasedOnFirst strat (x:xs) = do
  let tme = unsafePerformIO $ time x
  if tme < 0
    then return (x:xs)
    else (x :) <$> strat xs

chunkParList'' :: NFData a => PicoSeconds -> Chunker a -> Strategy [a]
chunkParList'' t c = chooseBasedOnFirst (chunkParList' t c)