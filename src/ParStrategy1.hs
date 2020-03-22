-- Idea of strategy: Have a predetermined 'optimal chunk time'
-- calculated, that says how long a chunk should optimally take
-- to evaluate. (valid approach? how to get this number?)
-- Then, when evaluating the list, calculate the time it takes to
-- evaluate the first element in the list and base the chunk size on that.
module ParStrategy1 ( parList ) where

import           Control.Parallel.Strategies    ( Strategy, NFData )
import           ChunkParList                   ( chunkParList )
import           Chunker

parList :: NFData a => Strategy [a]
parList = chunkParList chunker
  where chunker = measure one >-> repeatedly chunk
