reverseKGroup :: Int -> [a] -> [a]
reverseKGroup k = concat . map reverse . groupBy k
  where  groupBy x xs | null l    = []
                      | otherwise = l : groupBy x r
           where (l, r) = splitAt x xs
