module SICP
    (
    ) where

square :: Num a => a -> a
square n = n * n

larger :: Ord t => t -> t -> t
larger a b = if a > b then a else b

sumSqr :: (Ord a, Num a) => a -> a -> a -> a
sumSqr a b c
  | a > b && a > c = square a + square(larger b c)
  | b > a && b > c = square b + square(larger a c)
  | otherwise = square c + square(larger a b)
