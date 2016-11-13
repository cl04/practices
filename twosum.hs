import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)

fromList xs = IntMap.fromList (zip xs [0..]) -- ^ O(n)

twoSum :: Int -> [Int] -> Maybe (Int, Int)
twoSum t m = IntMap.foldlWithKey go Nothing intm
  where
    intm = fromList m
    go (Just x) k v = Just x
    go Nothing k v  = IntMap.lookup (t-k) intm >>= \v' -> return (v, v')
