-- A version of the library with a separate parList strategy
module Autopar
    ( Autopar.init
    , pFoldMap
    , pmap
    , pfilter
    )
where

import           Control.Parallel.Strategies
import qualified ParStrategy1                  as PS1
import qualified ParStrategy2                  as PS2

-- | Initialize the library, to start collect information about
--   the parallel execution. Should be called at start of program.
init :: IO ()
init = return () -- not implemented yet

strategy :: NFData a => Strategy [a]
strategy = PS1.parList'

-- | An automatically parallelizing map.
pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f xs = map f xs `using` strategy

-- | An automatically parallelizing filter.
pfilter :: NFData a => (a -> Bool) -> [a] -> [a]
pfilter pred' xs = filter pred' xs `using` strategy

-- | An automatically parallelizing foldMap.
pFoldMap :: (NFData m, Monoid m) => (a -> m) -> [a] -> m
pFoldMap f = mconcat . pmap f
