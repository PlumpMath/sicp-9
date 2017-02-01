
-- fib is a tree recursion example

fib :: Integral a => a -> a
fib a
  | a == 0 = 0
  | a == 1 = 1
  | otherwise = fib (a-1) + fib(a-2)
  
-- the iterative version
fib' :: Integral a => a -> a
fib' = fibIterator 1 0
  
fibIterator :: Integral a => a -> a -> a -> a
fibIterator a b c
  | c == 0 = b
  | otherwise = fibIterator (a+b) a (c-1)