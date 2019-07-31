-- Idea of strategy: Similar to ParStrategy1, but periodically
-- recalculate the chunk size. This to, in theory, be able to better
-- handle irregular lists.
module ParStrategy2 ( parList ) where

import           Control.Parallel.Strategies    ( Strategy, NFData )
import           ChunkParList                   ( chunkParList )
import           Chunker

parList :: NFData a => Strategy [a]
parList = chunkParList $ repeatedly (measure one >-> count 8 chunk)
