{-# LANGUAGE NumericUnderscores #-}
module Autopar
    ( Autopar.init
    , pFoldMap
    , pmap
    , pfilter
    )
where

import           Control.Parallel.Strategies
import           System.IO.Unsafe               ( unsafePerformIO )
import qualified Data.List.Split               as Split
import           GHC.Conc                       ( getNumCapabilities )
import qualified System.CPUTime                as CPUTime
import           Control.Exception              ( evaluate )
import           Control.DeepSeq                ( force )
import           Data.Tagged

data Pico
type PicoSeconds = Tagged Pico Integer

-- | The optimal time in picoseconds for a chunk to take
optChunkTime :: PicoSeconds
optChunkTime = 7_600_000_000

-- | Initialize the library, to start collect information about
--   the parallel execution. Should be called at start of program.
init :: IO ()
init = return () -- not implemented yet

-- | A version of foldMap that automatically parallelizes chunk-wise,
--   using the given chunk size.
pFoldMapChunk :: (NFData m, Monoid m) => Int -> Int -> (a -> m) -> [a] -> m
pFoldMapChunk t n f xs = mconcat mappedChunks
  where
    mappedChunks = concat (map f <$> chunks `using` parBuffer t rdeepseq)
    chunks       = Split.chunksOf n xs

-- | An automatically parallelizing foldMap.
pFoldMap :: (NFData m, Monoid m) => (a -> m) -> [a] -> m
{-# NOINLINE pFoldMap #-}
pFoldMap _ []       = mempty
pFoldMap f (x : xs) = unsafePerformIO $ do
    (b, execTime) <- time (f x)
    let chunkSize = fromIntegral (optChunkTime `div` max 1 execTime)
    nthreads <- getNumCapabilities
    return $ b <> pFoldMapChunk nthreads chunkSize f xs

-- | An automatically parallelizing map.
pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f = pFoldMap (pure . f)

-- | An automatically parallelizing filter.
pfilter :: NFData a => (a -> Bool) -> [a] -> [a]
pfilter pred' = pFoldMap (\x -> [ x | pred' x ])


---- HELPER FUNCTIONS ----

-- | Evaluate the argument to normal form and measure the execution time.
time :: NFData a => a -> IO (a, PicoSeconds)
time x = do
    startTime <- getCPUTime
    res       <- evaluate (force x)
    endTime   <- getCPUTime
    return (res, endTime - startTime)

-- | Wapper around CPUTime.getCPUTime
getCPUTime :: IO PicoSeconds
getCPUTime = Tagged <$> CPUTime.getCPUTime
