{-# LANGUAGE
    FlexibleContexts
  , MultiWayIf
  #-}

module Lib where


import Data.Maybe
import Control.Monad.State


data Marble = Red | Orange | Yellow | Green | Blue | Navy | Purple | Pink deriving (Read, Show, Eq)
type Bucket x = (x, Integer)


sortByBuckets (x:xs) = flip runState [] $ foldl op (ini x) xs
    where

        ini x = put ((x,1):[])

        op :: (Eq x, MonadState [Bucket x] m) => m () -> x -> m ()
        op m x = m >> do
            state <- get
            let keys = map fst state
            let key = listToMaybe $ filter (x ==) keys
            case key of
                Nothing     -> put $ (x, 1 :: Integer) : state
                Just key'   -> put $ applyToKey state key' increment
            return ()

            where
                increment = (+1)
                applyToKey :: (Eq x) => [(x,y)] -> x -> (y -> y) -> [(x,y)]
                applyToKey [] _ _ = []
                applyToKey ((x,y) : whatever) key function =
                    if x == key
                    then (x, function y) : applyToKey whatever key function
                    else (x, y) : applyToKey whatever key function

