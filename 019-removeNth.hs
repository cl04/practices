-- Remove Nth item from the end of list
-- Good example of foldr, and elegance of FP.

removeNthFromEnd :: Int -> [a] -> [a]
removeNthFromEnd n = snd . foldr go (1, [])
  where go x (k, xs) | k == n = (1+k, xs)
                     | k /= n = (1+k, x:xs)
