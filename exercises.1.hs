module SICP
    (
    ) where

square :: Num a => a -> a
square n = n * n

larger :: Ord t => t -> t -> t
larger a b = if a > b then a else b

sumSqr :: (Ord a, Num a) => a -> a -> a -> a
sumSqr a b c =
  if a > b && a > c then square a + square(larger b c)
  else
    if b > a && b > c then square b + square(larger a c)
    else
      square c + square(larger a b)
