module Patterns
    (
    ) where

pred' :: (Integral a) => a -> a
pred' n
  | n <= 0 = 0
  | otherwise = n - 1


map' :: Foldable t1 => (t -> a) -> t1 t -> [a]
map' f lista = foldr op [] lista
  where
    op x acc = f x : acc
