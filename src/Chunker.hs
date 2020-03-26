{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Chunker (
    Chunker
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
import           Data.Bool                      ( bool )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Util

newtype ChunkerT s r a = ChunkerT (StateT s (Reader r) a)
  deriving (Functor, Applicative, Monad, MonadState s, MonadReader r)
type Chunker a = ChunkerT (ChunkerState a) PicoSeconds [[a]]
data ChunkerState a = ChunkerState { chunkSize :: Int, list :: [a] }

instance Semigroup (Chunker a) where
  ch1 <> ch2 = liftA2 (++) ch1 ch2Checked
    where ch2Checked = null <$> gets list >>= bool ch2 mempty

instance Monoid (Chunker a) where
  mempty = return []

-- | Evaluate a Chunker and get the final value.
evalChunker :: Chunker a -> PicoSeconds -> [a] -> [[a]]
evalChunker (ChunkerT st) optTime =
  flip runReader optTime . evalStateT st . ChunkerState 8

-- | Chunker that extracts a chunk of the given size.
chunkOf :: Int -> Chunker a
chunkOf n = do
  (chnk, rest) <- splitAt n <$> gets list
  modify (\(ChunkerState size _) -> ChunkerState size rest)
  return [chnk]

-- | Chunker that extracts a chunk of size 1.
one :: Chunker a
one = chunkOf 1

-- | Chunker that creates a chunk, the size of the current value of chunkSize.
chunk :: Chunker a
chunk = gets chunkSize >>= chunkOf

-- | Chunks the remainder of the list, using the given Chunker repeatedly.
repeatedly :: Chunker a -> Chunker a
repeatedly = mconcat . repeat

-- | Apply a Chunker up to n times.
count :: Int -> Chunker a -> Chunker a
count n = mconcat . replicate n

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
