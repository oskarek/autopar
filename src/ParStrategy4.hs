-- Idea of strategy: Like ParStrategy2 but measure time of
-- more than one element and use their average.
module ParStrategy4 ( parList ) where

import           Control.Parallel.Strategies    ( Strategy, NFData )
import           ChunkParList                   ( chunkParList )
import           Chunker

parList :: NFData a => Strategy [a]
parList = chunkParList chunker
  where chunker = repeatedly (measure (chunkOf 8) >-> count 8 chunk)
