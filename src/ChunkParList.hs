module ChunkParList where

import           Control.Parallel.Strategies
import           System.IO.Unsafe               ( unsafePerformIO )
import           GHC.Conc                       ( getNumCapabilities )
import           Util
import           Chunker

-- | Create a lazy list evaluation strategy, that evaluates 
-- the list in chunks, according to the given Chunker.
chunkParList :: NFData a => Chunker a -> Strategy [a]
chunkParList chunker xs =
  let nthreads = unsafePerformIO getNumCapabilities
      chunks   = evalChunker chunker optChunkTime xs
  in  concat <$> parBuffer nthreads rdeepseq chunks
