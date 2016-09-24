{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , FlexibleInstances
  #-}

import Control.DeepSeq
import Criterion.Main
import GHC.Generics
import System.Random
import Test.QuickCheck

import Lib

instance Generic Marble
instance Generic (Buckets a)
instance NFData Marble where rnf = flip seq ()
instance (NFData a) => NFData (Buckets a) where rnf = flip seq ()

main :: IO ()
main = defaultMain [
    bgroup "buckets"
        [ bench "bucketize 10" $ nfIO $ fmap bucketize $ marbles 10
        , bench "sortByBuckets 10" $ nfIO $ fmap (sortByBuckets (==)) $ marbles 10
        , bench "bucketize 100" $ nfIO $ fmap bucketize $ marbles 100
        , bench "sortByBuckets 100" $ nfIO $ fmap (sortByBuckets (==)) $ marbles 100
        , bench "bucketize 1024" $ nfIO $ fmap bucketize $ marbles 1024
        , bench "sortByBuckets 1024" $ nfIO $ fmap (sortByBuckets (==)) $ marbles 1024
        ]
    ]



marbles n = sequence $ take n $ repeat (randomIO :: IO Marble)
