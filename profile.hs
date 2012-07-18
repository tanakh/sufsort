import qualified Data.Vector.Unboxed as U
import Data.SuffixArray

main :: IO ()
main = do
  let len = 1000000
      str = U.map fromEnum . U.fromList $ take len $ cycle "ABC"
      sa   = suffixSort str :: U.Vector Int
  print $ U.length sa
