{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  , OverlappingInstances
  , ScopedTypeVariables
  , MultiWayIf
  #-}

module Lib where


import              Control.Monad.State
import              Data.Bifunctor
import qualified    Data.FixedList      as FL
import qualified    Data.Foldable       as F
import qualified    Data.Map            as M
import              Data.Maybe
import              Data.Numbers.Primes
import qualified    Data.Set            as S
import              System.Random
import              Test.QuickCheck


data Marble = Red | Orange | Yellow | Green | Blue | Navy | Purple | Pink
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

type Code = FL.FixedList5 Marble
type Bucket x = (x, Integer)
newtype Buckets a = Buckets (M.Map a Integer) deriving Show

instance (Enum a, Bounded a) => Random a where
    randomR (lo,hi) g = bimap toEnum id $ randomR (fromEnum lo, fromEnum hi) g
    random g = randomR (minBound, maxBound) g

instance (Foldable f, Eq a) => Eq (f a) where
    (==) x y = (==) (F.toList x) (F.toList y)

instance (Ord a) => Monoid (Buckets a) where
    mempty = Buckets $ M.empty
    mappend (Buckets x) (Buckets y) = Buckets $ M.unionWith (+) x y


marbles :: Int -> IO [Marble]
marbles n = sequence $ take n $ repeat (randomIO :: IO Marble)

genCode :: IO Code
genCode = fmap (FL.fromFoldable' . take 5) $ generate $ shuffle [(minBound :: Marble) .. maxBound]

bucketize :: (Foldable f, Eq a, Ord a) => f a -> Buckets a
bucketize fa = foldMap (\x -> Buckets $ M.singleton x 1) fa

