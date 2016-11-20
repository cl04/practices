letterCombinations :: String -> [String]
letterCombinations [] = [[]]
letterCombinations (x:xs) = case lookup x dict of
  Nothing -> []
  Just ys -> [ a:b | a <- ys, b <- letterCombinations xs]
  where dict = [ ('2', "abc")       , ('3', "def")
               , ('4', "ghi")       , ('5', "jkl")
               , ('6', "mno")       , ('7', "pqrs")
               , ('8', "tuv")       , ('9', "wxyz") ]
