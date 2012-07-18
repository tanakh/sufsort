{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Criterion
import Criterion.Main
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign
import Foreign.C
import System.Random

import Data.SuffixArray

toV :: String -> U.Vector Word8
toV = U.map (fromIntegral . fromEnum) . U.fromList

suffixSort' :: U.Vector Word8 -> U.Vector Int
suffixSort' = suffixSort

saisC :: U.Vector Word8 -> IO (U.Vector Int)
saisC s = do
  let n = U.length s
  ss <- S.unsafeThaw (G.convert s)
  SM.unsafeWith ss $ \ps -> do
    psa <- mallocForeignPtrArray n
    withForeignPtr psa $ \ppsa -> do
      r <- c_sais ps ppsa (fromIntegral n) 256
      assert (r == 0) $ return ()
    G.convert <$> S.unsafeFreeze (SM.unsafeFromForeignPtr0 psa n)

divSufSort :: U.Vector Word8 -> IO (U.Vector Int)
divSufSort s = do
  let n = U.length s
  ss <- S.unsafeThaw (G.convert s)
  SM.unsafeWith ss $ \ps -> do
    psa <- mallocForeignPtrArray n
    withForeignPtr psa $ \ppsa -> do
      r <- c_divsufsort ps ppsa (fromIntegral n)
      assert (r == 0) $ return ()
    G.convert <$> S.unsafeFreeze (SM.unsafeFromForeignPtr0 psa n)

main :: IO ()
main = do
  let len = 10000
  !strA    <- toV <$> (return $ replicate len 'A')
  !strAB   <- toV <$> (replicateM len $ randomRIO ('A', 'B'))
  !strLoop <- toV <$> (return $ take len $ cycle "ABC")
  !strRand <- toV <$> (replicateM len $ randomRIO ('A', 'Z'))

  defaultMain
    [ bgroup "sufsort"
      [ bgroup "A"
        [ bench "this"       $ whnf suffixSort'    strA
        , bench "sais"       $ whnfIO $ saisC      strA
        , bench "divsufsort" $ whnfIO $ divSufSort strA
        ]
      , bgroup "AB"
        [ bench "this"       $ whnf suffixSort'    strAB
        , bench "sais"       $ whnfIO $ saisC      strAB
        , bench "divsufsort" $ whnfIO $ divSufSort strAB
        ]
      , bgroup "Loop"
        [ bench "this"       $ whnf suffixSort'    strLoop
        , bench "sais"       $ whnfIO $ saisC      strLoop
        , bench "divsufsort" $ whnfIO $ divSufSort strLoop
        ]
      , bgroup "Rand"
        [ bench "this"       $ whnf suffixSort'    strRand
        , bench "sais"       $ whnfIO $ saisC      strRand
        , bench "divsufsort" $ whnfIO $ divSufSort strRand
        ]
      ]
    ]

foreign import ccall "sais" c_sais
  :: Ptr Word8 -> Ptr Int -> CInt -> CInt -> IO CInt
foreign import ccall "divsufsort" c_divsufsort
  :: Ptr Word8 -> Ptr Int -> CInt -> IO CInt
