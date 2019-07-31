-- Idea of strategy: Like ParStrategy1 but measure time of
-- more than one element at the start and use their average.
module ParStrategy3 ( parList ) where

import           Control.Parallel.Strategies    ( Strategy, NFData )
import           ChunkParList                   ( chunkParList )
import           Chunker

parList :: NFData a => Strategy [a]
parList = chunkParList $ measure (chunkOf 8) >-> repeatedly chunk
