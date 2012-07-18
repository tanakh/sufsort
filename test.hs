import Data.List
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.Hspec.QuickCheck

import Data.SuffixArray

naive :: String -> [Int]
naive ss = map snd . sort $ zip (init $ tails ss) [0..]

main :: IO ()
main = hspec $ do
  describe "sais" $ do
    it "test case" $ do
      let str = "mmiissiissiippii"
          ans = [15, 14, 10, 06, 02, 11, 07, 03, 01, 00, 13, 12, 09, 05, 08, 04]
          sa  = suffixSort . U.map fromEnum . U.fromList $ str
      sa `shouldBe` U.fromList (ans :: [Int])

    prop "sort correctly" $ \str ->
      if length str >= 2 then
        let sa = suffixSort $ U.fromList $ map fromEnum str
        in U.toList sa == naive str
      else True
