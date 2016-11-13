{-# LANGUAGE MultiWayIf #-}

import Control.Applicative

import Data.Maybe

(+?) :: Integral a => Maybe a -> Maybe a -> Maybe a
(+?) x y = liftA2 (+) x y <|> x <|> y

uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

safeHead = listToMaybe

tail_ [] = []
tail_ (x:xs) = xs

add2_ c l r = case c +? safeHead l +? safeHead r of
  Nothing -> []
  Just s  -> if | s >= 10 -> (s - 10) : add2_ (Just 1) (tail_ l) (tail_ r)
                | s  < 10 ->       s  : add2_ Nothing  (tail_ l) (tail_ r)

addTwo :: [Int] -> [Int] -> [Int]
addTwo xs = add2_ (Just 0) xs
