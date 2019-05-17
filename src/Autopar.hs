{-# LANGUAGE StandaloneDeriving #-}
module Autopar
    ( Autopar.init
    , pFoldMap
    , pmap
    , pfilter
    )
where

import qualified Control.Concurrent.STM        as STM
import           Control.Parallel.Strategies
import           System.IO.Unsafe               ( unsafePerformIO )
import           GHC.Stack
import qualified Data.List                     as List
import qualified Data.Map                      as M
import qualified Data.Maybe                    as May
import qualified Data.Set                      as Set
import qualified Data.List.Split               as Split
import           GHC.Conc                       ( getNumCapabilities )

-- Temporary fix for being able to use SrcLoc as keys in a Map
deriving instance Ord SrcLoc


type RuntimeInfo = Set.Set (Int, Int) -- Temporary data structure for runtime info
type RuntimeMap = M.Map SrcLoc RuntimeInfo

runtimeInfo :: STM.TVar RuntimeMap
{-# NOINLINE runtimeInfo #-}
runtimeInfo = unsafePerformIO (STM.newTVarIO M.empty)

-- | Insert a new measurement in the global runtime map.
insertNewMeasurement :: SrcLoc -> Int -> Int -> IO ()
insertNewMeasurement srcLoc n time = STM.atomically (STM.modifyTVar runtimeInfo adjust)
    where adjust = M.adjust (Set.insert (n, time)) srcLoc

-- | Get the chunk size to use in the next evaluation,
--   based on stored info from previous runs.
getNewChunkSize :: RuntimeMap -> SrcLoc -> Int
getNewChunkSize rtm s = maybe 1 getNewChunkSize' (rtm M.!? s)

getNewChunkSize' :: RuntimeInfo -> Int
getNewChunkSize' inf | null inf  = 1
                     | otherwise = 100 -- dummy value

-- | Initialize the library, to start collect information about
--   the parallel execution. Should be called at start of program.
init :: IO ()
init = return () -- dummy implementation

-- | A version of foldMap that automatically paralellizes chunk-wise,
--   using the given chunk size.
pFoldMapChunk :: (NFData m, Monoid m) => Int -> Int -> (a -> m) -> [a] -> m
pFoldMapChunk _ n f xs | n < 3 = mconcat (map f xs `using` parList rdeepseq)
pFoldMapChunk t n f xs         = mconcat mappedChunks
  where
    mappedChunks = concat (map (map f) chunks `using` parBuffer t rdeepseq)
    chunks       = Split.chunksOf n xs

-- | An automatically paralellizing foldMap.
pFoldMap :: (HasCallStack, NFData m, Monoid m) => (a -> m) -> [a] -> m
{-# NOINLINE pFoldMap #-}
pFoldMap f t = unsafePerformIO $ do
    runtimeMap <- STM.readTVarIO runtimeInfo
    threads    <- getNumCapabilities
    let srcLoc = callingSrcLoc
        n      = getNewChunkSize runtimeMap callingSrcLoc
        res    = pFoldMapChunk threads n f t
        time   = 1500 -- dumy value - should be measured
    print threads
    insertNewMeasurement srcLoc n time
    return res

-- | An automatically paralellizing map.
pmap :: (HasCallStack, NFData b) => (a -> b) -> [a] -> [b]
pmap f = pFoldMap (pure . f)

-- | An automatically paralellizing filter.
pfilter :: (HasCallStack, NFData a) => (a -> Bool) -> [a] -> [a]
pfilter pred' = pFoldMap (\x -> [ x | pred' x ])


---- HELPER FUNCTIONS ----
callingSrcLoc :: HasCallStack => SrcLoc
callingSrcLoc = May.fromJust
    $ List.find ((/= "Autopar") . srcLocModule)
    $ List.map snd (getCallStack callStack)