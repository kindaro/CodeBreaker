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


instance (Enum a, Bounded a) => Random a where
    randomR (lo,hi) g = bimap toEnum id $ randomR (fromEnum lo, fromEnum hi) g
    random g = randomR (minBound, maxBound) g

instance (Foldable f, Eq a) => Eq (f a) where
    (==) x y = (==) (F.toList x) (F.toList y)


data Marble = Red | Orange | Yellow | Green | Blue | Navy | Purple | Pink
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

type Code = FL.FixedList5 Marble

type Bucket x = (x, Integer)

newtype Buckets a = Buckets (M.Map a Integer) deriving Show

instance (Ord a) => Monoid (Buckets a) where
    mempty = Buckets $ M.empty
    mappend (Buckets x) (Buckets y) = Buckets $ M.unionWith (+) x y

marbles :: Int -> IO [Marble]
marbles n = sequence $ take n $ repeat (randomIO :: IO Marble)

genCode :: IO Code
genCode = fmap (FL.fromFoldable' . take 5) $ generate $ shuffle [(minBound :: Marble) .. maxBound]

bucketize :: (Foldable f, Eq a, Ord a) => f a -> Buckets a
bucketize fa = foldMap (\x -> Buckets $ M.singleton x 1) fa

bucketize_State eq (x:xs) = snd $ flip runState [] $ foldl op (ini x) xs
    where

        ini x = put ((x,1):[])

        -- op :: (Eq x, MonadState [Bucket x] m) => m () -> x -> m ()
        op m x = m >> do
            state <- get
            let keys = map fst state
            let key = listToMaybe $ filter (x `eq`) keys
            case key of
                Nothing     -> put $ (x, 1 :: Integer) : state
                Just key'   -> put $ applyToKey state key' increment
            return ()

            where
                increment = (+1)
                -- applyToKey :: (Eq x) => [(x,y)] -> x -> (y -> y) -> [(x,y)]
                applyToKey [] _ _ = []
                applyToKey ((x,y) : whatever) key function =
                    if x `eq` key
                    then (x, function y) : applyToKey whatever key function
                    else (x, y) : applyToKey whatever key function

newtype P = P (Integer, Integer) deriving Show
instance Monoid P where
    mempty = P(1,1)
    mappend (P(x1, y1)) (P(x2, y2)) = P ((x1 * x2), (y1 * y2))

distributeP :: [a] -> Integer -> [(a, P)]
distributeP list p = map (\x -> (x, P (p, (fromIntegral . FL.length) list))) list

simplify :: P -> P
simplify (P(xs,ys)) = undefined
    where
        keys = map fst
        common xs ys = S.elems $ (S.fromList xs) `S.intersection` (S.fromList ys)
        commonKeys xs ys = common (keys xs) (keys ys)

s1 =    [ [ Red, Orange, Yellow, Green, Blue ]
        , [ Red, Yellow, Navy, Purple, Pink ]
        , [ Green, Blue, Navy, Purple, Pink ]
        , [ Red, Blue, Navy, Purple, Pink ]
        , [ Orange, Blue, Navy, Purple, Pink ]
        , [ Red, Green, Navy, Purple, Pink ]
        , [ Pink, Red, Blue, Green, Purple ]
        ]

s1' = concat $ map (uncurry distributeP) $ zip s1 [3,3,4,4,3,4,4]

s1_no = concat . map (uncurry distributeP) $
    [ ([ Red, Orange, Yellow, Green, Blue ], 3)
    , ([ Red, Yellow, Navy, Purple, Pink ], 3)
    , ([ Green, Blue, Navy, Purple, Pink ], 4)
    , ([ Red, Blue, Navy, Purple, Pink ], 4)
    , ([ Orange, Blue, Navy, Purple, Pink ], 3)
    , ([ Red, Green, Navy, Purple, Pink ], 4)
    , ([ Pink, Red, Blue, Green, Purple ], 4)
    ]

