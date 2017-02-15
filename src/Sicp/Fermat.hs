module Sicp
    (
    ) where

import           System.IO.Unsafe
import           System.Random

square :: Integral a => a -> a
square n = n * n

expmod :: Integral a => a -> a -> a -> a
expmod base e m
  | e == 0 = 1
  | even e = square (f quot 2) `mod` m
  | otherwise = base * f (-) 1 `mod` m
  where
    f f' n = expmod base (f' e n) m

fermat n = test (unsafePerformIO $ randomRIO (1, n))
  where
    test rnd = expmod rnd n n == rnd
