{-# LANGUAGE NumericUnderscores #-}
module Common where

import           Control.Parallel.Strategies
import           System.IO.Unsafe               ( unsafePerformIO )
import           GHC.Conc                       ( getNumCapabilities )
import           Util
import           Data.Tagged                    ( Tagged(..) )

data NamedStrategy a = NamedStrategy { name :: String, strat :: Strategy a }

listLengths :: [Int]
listLengths = [10, 100, 1_000, 10_000, 100_000, 1_000_000]

chunkTimes :: [PicoSeconds]
chunkTimes =
  Tagged
    <$> [ --1_000_000
        -- , 10_000_000
        -- , 100_000_000
         1_000_000_000
        -- , 10_000_000_000
        -- , 100_000_000_000
        -- , 1_000_000_000_000
        ]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs

chunkParBuffer :: NFData a => Int -> Strategy [a]
chunkParBuffer chunkSize xs =
  let nthreads = unsafePerformIO getNumCapabilities
      chunks   = chunk chunkSize xs
  in  concat <$> parBuffer (2*nthreads) rdeepseq chunks