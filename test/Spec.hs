{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , FlexibleInstances
  #-}

import Control.DeepSeq
import Criterion.Main as C
import Data.List as L
import Data.Map as M
import GHC.Generics
import System.Random
import Test.QuickCheck

import Lib

instance Generic Marble
instance Generic (Buckets a)
instance NFData Marble where rnf = flip seq ()
instance (NFData a) => NFData (Buckets a) where rnf = flip seq ()

instance Arbitrary Marble where
    arbitrary = choose (minBound, maxBound)

main :: IO ()
main = do
    quickCheck prop_compareBucketize
    C.defaultMain [
        bgroup "buckets"
            [ bench "bucketize 10" $ nfIO $ fmap bucketize $ marbles 10
            , bench "bucketize_State 10" $ nfIO $ fmap (bucketize_State (==)) $ marbles 10
            , bench "bucketize 1024" $ nfIO $ fmap bucketize $ marbles 1024
            , bench "bucketize_State 1024" $ nfIO $ fmap (bucketize_State (==)) $ marbles 1024
            ]
        ]

prop_compareBucketize = forAll (vector 10 :: Gen [Marble]) $
    \x -> (val_bucketize x) == (val_bucketize_State x)
    where
        val_bucketize = L.sort . ((\(Buckets x) -> M.toList x) . bucketize)
        val_bucketize_State = L.sort . (bucketize_State (==))
