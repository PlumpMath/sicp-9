
module BuildExamples
    (
    ) where

import           GHC.Exts

cube :: Integral a => a -> a
cube n = n * n * n

mapCube :: Integral a => [a] -> [a]
mapCube xs = build (\c n -> foldr (c . cube) n xs)

sum :: Integral a => [a] -> a
sum =foldr (+) 0

-- sum (mapCube xs) = foldr (+) 0 (build (\c n -> foldr (n . cube) n xs))

sum' :: Integral a => [a] -> a
sum' xs = (\c n -> foldr (c . cube) n xs) (+) 0

-- sum'' :: Integral a => [a] -> a
-- sum'' :: Integral a => [a] -> a
sum'' xs = foldr ((+) . cube) 0 xs
