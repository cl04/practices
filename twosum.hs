import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)

twoSum :: Int -> [Int] -> Maybe (Int, Int)
twoSum t m = IntMap.foldlWithKey go Nothing intm
  where
    intm = IntMap.fromList (zip m [0..]) -- ^O(n)
    go (Just x) k v = Just x
    go Nothing k v  = IntMap.lookup (t-k) intm >>= \v' -> return (v, v')
