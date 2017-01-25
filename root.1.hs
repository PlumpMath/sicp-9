module Root
    (
    ) where

square :: Num a => a -> a
square n = n * n

abs' :: (Ord t, Num t) => t -> t
abs' x
  | x < 0 = -x
  | otherwise = x

eps :: Double
eps = 0.00001

until' :: (t -> Bool) -> (t -> t) -> t -> t
until' cond f x = if cond x then x else until' cond f (f x)

deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f(x + eps) - f x) / eps

newton :: (Double -> Double) -> Double -> Double
newton f = until' isGoodEnough improve
  where isGoodEnough y = abs'(f y) < eps
        improve y      = y - (f y / deriv f y)

sqrt' :: Double -> Double
sqrt' x = newton f x
  where f y = square y - x

cubeRoot :: Double -> Double
cubeRoot x = newton f x
  where f y = (y * y * y) - x
