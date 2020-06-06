module Bonus where

import           CSV

import qualified Data.HashMap.Strict   as HS
import           System.Random
import           System.Random.Shuffle

getBonusWinners :: RandomGen g => g -> [DB] -> [String]
getBonusWinners _   []                   = []
getBonusWinners gen (current : previous) = excluded
  where
    (winners : rest) = previous
    winnerIDs        = map fst $ HS.toList winners
    curList          = HS.toList current
    rawCandidates    = map fst $ filter isCandidate curList
    candidates       = safeShuffle rawCandidates gen
    (gen', _)        = split gen
    excluded         = if null previous then candidates else
      filter (`notElem` winnerIDs) candidates ++ getBonusWinners gen' rest

isCandidate :: (String, [String]) -> Bool
isCandidate (_, vs) = 2 `elem` fights && 1 `notElem` fights
  where
    pairs  = map read vs :: [(Int, Int)]
    fights = map fst pairs

safeShuffle :: RandomGen g => [a] -> g -> [a]
safeShuffle [] _ = []
safeShuffle as g = shuffle' as (length as) g
