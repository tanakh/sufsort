{-# LANGUAGE ConstraintKinds, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.SuffixArray (
  suffixSort,
  sais,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Monad.Trans.Loop
import Data.Bits
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

import Debug.Trace

type CharType a = (Num a, Integral a, Bits a, Bounded a, U.Unbox a)
type IndexType a = (Num a, Integral a, Bits a, Bounded a, U.Unbox a)

-- | Sort suffixes for given string.
-- | String must not contain the `maxBound :: b` values.
suffixSort :: ( CharType a, IndexType b
              , G.Vector v a, G.Vector v b
              , G.Vector v (Int, b), G.Vector v Bool, Show (v Bool), Show (v b), Show b )
              => v a -> v b
suffixSort s = runST $ do
  sa <- GM.replicate (G.length s) 0
  sais s sa $ fromIntegral (G.maximum s) + 1
  G.unsafeFreeze sa

sais :: forall a b m s v w.
        ( CharType a, G.Vector v a
        , IndexType b, GM.MVector w b
        , G.Vector v b, G.Vector v Bool, G.Vector v (Int, b), Show (v Bool), Show (v b), Show b
        , w ~ G.Mutable v
        , Functor m, PrimMonad m, s ~ PrimState m )
        => v a -> w s b -> b -> m ()
sais t sa k = do
  let n = G.length t

  -- create LS-string. L: False, S: True (L < S)
  let ls = G.create $ do
        mls <- GM.replicate n False
        iterateLoopT (n - 2, G.last t, False) $ \(i, n1, b) -> do
          let n0 = t G.! i
              c | n0 < n1 = True
                | n0 > n1 = False
                | otherwise = b
          lift $ GM.write mls i c
          when (i == 0) exit
          return (i - 1, n0, c)
        return mls

  let isLMS ix =
        ix /= maxBound && ix > 0 && (ls G.! ix) && not (ls G.! (ix - 1))

  -- Stage1:reduce the problem by at least 1/2
  -- sort all the S-substrings
  bkt <- G.thaw $ getBucket t k True
  GM.set sa maxBound
  forM_ [1..n-1] $ \i -> do
    when (isLMS i) $ do
      ix <- pred <$> GM.read bkt (fromIntegral $ t G.! i)
      GM.write sa (fromIntegral ix) $ fromIntegral i
      GM.write bkt (fromIntegral $ t G.! i) ix

  induceSAl t sa k ls
  (\s -> traceShow (s :: v b) $ return ()) =<< G.unsafeFreeze sa
  induceSAs t sa k ls
  (\s -> traceShow (s :: v b) $ return ()) =<< G.unsafeFreeze sa

  -- compact all the sorted substrings into the first n1 items of SA
  -- 2*n1 must be not larger than n (proveable)
  n1 <- iterateLoopT (0, 0) $ \(i, n1) -> do
    when (i >= n) $ exitWith n1
    sai <- lift $ GM.read sa i
    when (isLMS $ fromIntegral sai) $ do
      lift $ GM.write sa n1 sai
      continueWith (i + 1, n1 + 1)
    continueWith (i + 1, n1)

  -- find the lexicographic names of all substrings
  forM_ [n1 .. n-1] $ \i -> GM.write sa i maxBound
  name <- iterateLoopT (0, 0, maxBound) $ \(i, name, prev) -> do
    when (i >= n1) $ exitWith name
    pos <- lift $ fromIntegral <$> GM.read sa i
    diff <- iterateLoopT 0 $ \d -> do
      when (d >= n) $ exitWith False
      when (prev == maxBound ||
            t G.! (pos + d) /= t G.! (prev + d) ||
            ls G.! (pos + d) /= ls G.! (prev + d)) $
        exitWith True
      when (d > 0 && (isLMS (pos + d) || isLMS (prev + d))) $
        exitWith False
      return $ d + 1
    traceShow diff $ return ()
    let (nname, nprev) = if diff then (name + 1, pos) else (name, prev)
        p = if even pos then pos `div` 2 else (pos - 1) `div` 2
    lift $ GM.write sa (n1 + p) $ nname - 1
    return (i + 1, nname, nprev)

  iterateLoopT (n - 1, n - 1) $ \(i, j) -> do
    when (i < n1) exit
    sai <- lift $ GM.read sa i
    when (sai /= maxBound) $ do
      lift $ GM.write sa j sai
      continueWith (i - 1, j - 1)
    return (i - 1, j)

  -- stage 2: solve the reduced problem
  let sa1 = GM.take n1 sa
      s1  = GM.drop (n - n1) sa
  if fromIntegral name < n1
    then do -- recurse if names are not yet unique
    is1 <- G.freeze s1
    traceShow is1 $ return ()
    sais (is1 :: v b) sa1 name
    else do -- generate the suffix array of s1 directly
    forM_ [0 .. n1 - 1] $ \i -> do
      ix <- fromIntegral <$> GM.read s1 i
      GM.write sa1 ix (fromIntegral i)

  -- stage 3: induce the result for the original problem
  bkt <- G.thaw $ getBucket t k True
  iterateLoopT (1, 0) $ \(i, j) -> do
    when (i >= n) exit
    when (isLMS i) $ do -- get p1
      lift $ GM.write s1 j $ fromIntegral i
      continueWith (i + 1, j + 1)
    return (i + 1, j)

  forM_ [0 .. n1 - 1] $ \i -> do
    ix <- fromIntegral <$> GM.read sa1 i
    GM.write sa1 i =<< GM.read s1 ix

  -- init SA[n1..n-1]
  forM_ [n1 .. n - 1] $ \i -> do
    GM.write sa i maxBound

  forM [n1 - 1, n1 - 2 .. 0] $ \i -> do
    j <- fromIntegral <$> GM.read sa i
    GM.write sa i maxBound
    ix <- pred <$> GM.read bkt (fromIntegral $ t G.! j)
    GM.write bkt (fromIntegral $ t G.! j) ix
    GM.write sa (fromIntegral ix) (fromIntegral j)

  induceSAl t sa k ls
  induceSAs t sa k ls

induceSAl :: ( Functor m, PrimMonad m, s ~ PrimState m
             , CharType a, IndexType b
             , G.Vector v a, G.Vector v b, G.Vector v (Int, b), G.Vector v Bool
             , GM.MVector w b )
             => v a -> w s b -> b -> v Bool -> m ()
induceSAl t sa k ls = do
  bkt <- G.thaw $ getBucket t k False
  -- for last letter
  let n = G.length t
  ix0 <- GM.read bkt (fromIntegral $ t G.! (fromIntegral $ n - 1))
  GM.write bkt (fromIntegral $ t G.! (fromIntegral $ n - 1)) $ ix0 + 1
  GM.write sa (fromIntegral ix0) (fromIntegral $ n - 1)
  -- rest
  foreach [0 .. G.length t - 1] $ \i -> do
    j0 <- lift $ GM.read sa i
    let j = fromIntegral $ j0 - 1
    when (j0 == maxBound || j0 == 0) continue
    lift $ when (not (ls G.! j)) $ do
      ix <- GM.read bkt (fromIntegral $ t G.! j)
      GM.write bkt (fromIntegral $ t G.! j) (fromIntegral $ ix + 1)
      GM.write sa (fromIntegral ix) (fromIntegral j)

induceSAs :: ( Functor m, PrimMonad m, s ~ PrimState m
             , CharType a, IndexType b
             , G.Vector v a, G.Vector v b, G.Vector v (Int, b), G.Vector v Bool
             , GM.MVector w b )
             => v a -> w s b -> b -> v Bool -> m ()
induceSAs t sa k ls = do
  bkt <- G.thaw $ getBucket t k True
  let n = G.length t
  foreach [n - 1, n - 2 .. 0] $ \i -> do
    j0 <- lift $ GM.read sa i
    let j = fromIntegral $ j0 - 1
    when (j0 == maxBound || j0 == 0) continue
    lift $ when (j >= 0 && ls G.! j) $ do
      ix <- pred <$> GM.read bkt (fromIntegral $ t G.! j)
      GM.write bkt (fromIntegral $ t G.! j) ix
      GM.write sa (fromIntegral ix) (fromIntegral j)

getBucket :: ( CharType a, G.Vector v a
             , IndexType b, G.Vector v b
             , G.Vector v (Int, b) )
             => v a -> b -> Bool -> v b
getBucket t k end =
  (if end then G.postscanl' else G.prescanl') (+) 0
  $ G.accumulate (+) (G.replicate (fromIntegral k) 0)
  $ G.map (\c -> (fromIntegral c, 1))
  $ t
