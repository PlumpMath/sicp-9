



factorial :: Integral a => a -> a
factorial a 
  | a < 1 = 0
  | a == 1 = 1
  | otherwise = a * factorial (a - 1)
  

factorial' :: Integral a => a -> a
factorial' a
  | a == 0 = 1
  | otherwise = factIterator 1 1 a


factIterator :: Integral a => a -> a -> a -> a
factIterator ac counter mx = if counter > mx 
                              then ac
                              else factIterator (ac*counter) (counter+1) mx