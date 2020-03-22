module Chunker (
    Chunker
  , (>->)
  , evalChunker
  , chunkOf
  , one
  , chunk
  , repeatedly
  , count
  , measure
) where

import           Control.Applicative            ( liftA2 )
import           Control.DeepSeq                ( NFData )
import           Control.Monad.State
import           Control.Monad.Reader
import           System.IO.Unsafe               ( unsafePerformIO )
import           Util

type Chunker a = StateT (ChunkerState a) (Reader PicoSeconds) [[a]]
data ChunkerState a = ChunkerState { chunkSize :: Int, list :: [a] }

infixr 1 >->

-- | Compose two chunkers.
(>->) :: Chunker a -> Chunker a -> Chunker a
(>->) = liftA2 (++)

-- | Evaluate a Chunker and get the final value.
evalChunker :: Chunker a -> PicoSeconds -> [a] -> [[a]]
evalChunker ch optTime =
  fst . flip runReader optTime . runStateT ch . ChunkerState 8

-- | Create a Chunker that extracts a chunk of the given size.
chunkOf :: Int -> Chunker a
chunkOf n = do
  (chnk, rest) <- splitAt n <$> gets list
  modify (\(ChunkerState size _) -> ChunkerState size rest)
  return [chnk]

-- | Creates a chunk of the first element.
one :: Chunker a
one = chunkOf 1

-- | Creates a chunk, the size of the current value of chunkSize.
chunk :: Chunker a
chunk = gets chunkSize >>= chunkOf

-- | Chunks the remainder of the list, using the given Chunker repeatedly.
repeatedly :: Chunker a -> Chunker a
repeatedly chunker = do
  done <- null <$> gets list
  if done then pure [] else chunker >-> repeatedly chunker

-- | Apply a Chunker up to n times.
count :: Int -> Chunker a -> Chunker a
count n chunker = do
  done <- null <$> gets list
  if n <= 0 || done then pure [] else chunker >-> count (n - 1) chunker

-- | Applies the given Chunker, measures how long it takes to
-- evaluate the contained elements to normal form, and updates
-- the chunkSize value based on that evaluation time.
measure :: NFData a => Chunker a -> Chunker a
measure chunker = chunker >>= \chunks -> do
  optTime <- ask
  let chunkSize' = unsafePerformIO $ calcNewChunkSize optTime (concat chunks)
  modify (\(ChunkerState _ rest) -> ChunkerState chunkSize' rest)
  return chunks

-- Calculate a new chunk size based on the evaluation time
-- of the given list of elements.
calcNewChunkSize :: NFData a => PicoSeconds -> [a] -> IO Int
calcNewChunkSize opt xs = do
  avgTime <- avg <$> traverse time xs
  return $ fromIntegral (max 1 (opt `div` max 1 avgTime))
