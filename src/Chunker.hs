module Chunker where

import           Control.DeepSeq                ( NFData )
import           Control.Monad.State
import           Control.Monad.Reader
import           System.IO.Unsafe               ( unsafePerformIO )
import           Util
import           Control.Applicative            ( liftA2 )
import           Data.Bool                      ( bool )

type Chunker a = StateT (ChunkData a) (ReaderT PicoSeconds IO) [[a]]
data ChunkData a = CD { chunkSize :: Int, list :: [a] } deriving (Eq, Show)

infixl 1 >->

-- | Compose two chunkers
(>->) :: Chunker a -> Chunker a -> Chunker a
(>->) = liftA2 (++)

-- | Unwrap the Chunker monad computation as a function.
runChunker :: Chunker a -> PicoSeconds -> [a] -> ([[a]], ChunkData a)
runChunker ch optTime =
  unsafePerformIO . flip runReaderT optTime . runStateT ch . CD 8

-- | Evaluate a Chunker and get the final value.
evalChunker :: Chunker a -> PicoSeconds -> [a] -> [[a]]
evalChunker ch optTime = fst . runChunker ch optTime

-- | Create a Chunker that extracts a chunk of the given size.
chunkOf :: Int -> Chunker a
chunkOf n = splitAt n <$> gets list >>= \(chnk, rest) ->
  modify (\(CD s _) -> CD s rest) >> pure [chnk]

-- | Creates a chunk of the first element.
one :: Chunker a
one = chunkOf 1

-- | Creates a chunk, the size of the current value of chunkSize.
chunk :: Chunker a
chunk = gets chunkSize >>= chunkOf

-- | Chunks the entire list, using the given Chunker repeatedly.
repeatedly :: Chunker a -> Chunker a
repeatedly ch = null <$> gets list >>= bool (ch >-> repeatedly ch) (pure [])

-- | Apply a Chunker up to n times.
count :: Int -> Chunker a -> Chunker a
count 0 _ = pure []
count n ch = null <$> gets list >>= bool (ch >-> count (n-1) ch) (pure [])

-- | Applies the given Chunker, measures how long it takes to
-- evaluate the contained elements to normal form, and updates
-- the chunkSize value based on that evaluation time.
measure :: NFData a => Chunker a -> Chunker a
measure chunker = chunker >>= \chunks -> do
  optTime    <- ask
  chunkSize' <- liftIO $ calcNewChunkSize optTime (concat chunks)
  modify (\(CD _ l) -> CD chunkSize' l)
  return chunks

-- Calculate a new chunk size based on the evaluation time
-- of the given list of elements.
calcNewChunkSize :: NFData a => PicoSeconds -> [a] -> IO Int
calcNewChunkSize opt xs = do
  avgTime <- avg <$> traverse time xs
  return $ fromIntegral (max 1 (opt `div` max 1 avgTime))
