module Exercises
    (
    ) where

f :: Integral n => n -> n
f n
  | n < 3 = n
  | otherwise = f(n -  1) + 2 * f(n - 2) + 3 * f(n - 3)

helper :: Integral i => [i] -> [i]
helper is
  | null is = []
  | otherwise = [ x + y | x <- is, y <- tail is]

reduce :: [Int] -> [Int]
reduce (x:rest@(y:z)) = (x + y) : (reduce rest)
reduce _              = []
