import Data.List
import qualified Data.Vector.Unboxed as U
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
-- import Test.QuickCheck

import Data.SuffixArray

naive :: String -> [Int]
naive ss = map snd . sort $ zip (init $ tails ss) [0..]

main :: IO ()
main = hspec $ do
  describe "sais" $ do
    -- let str = "mmiissiissiippii"
    prop "sort correctly" $ \str ->
      if length str >= 2 then
        let sa = suffixSort $ U.fromList $ map fromEnum str
        in U.toList sa == naive str
      else True
