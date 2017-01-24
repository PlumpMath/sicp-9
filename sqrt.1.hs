module SQRT
    (
    ) where

-- √x = the y such that y≥0 and y2=x.

square :: Num a => a -> a
square n = n * n

abs' :: (Ord t, Num t) => t -> t
abs' x
  | x < 0 = -x
  | otherwise = x

eps :: Double
eps = 0.00001

improve :: Fractional a => a -> a -> a
improve x y = (y + x / y) / 2

isGoodEnough :: Double -> Double -> Bool
isGoodEnough x y = abs' (square y - x) < eps

until' :: (t -> Bool) -> (t -> t) -> t -> t
until' cond f x = if cond x then x else until' cond f (f x)

sqrt' :: Double -> Double
sqrt' x = until' (isGoodEnough x) (improve x) x
