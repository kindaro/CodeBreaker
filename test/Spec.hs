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
    C.defaultMain [
        bgroup "buckets"
            [ bench "bucketize 10" $ nfIO $ fmap bucketize $ marbles 10
            , bench "bucketize 1024" $ nfIO $ fmap bucketize $ marbles 1024
            ]
        ]
