{-# LANGUAGE UnicodeSyntax #-}

module Bonus
  ( getBonusWinners
  ) where

import           DataWithFile
import           Types

import qualified Data.HashMap.Strict   as HS
import           System.Random         (RandomGen, split)
import           System.Random.Shuffle (shuffle')

getBonusWinners ∷ World → [Int]
getBonusWinners world =
  getBonusWinners' (_stdGen world) (_fightDBs world) (_winnersLists world)

getBonusWinners' ∷ RandomGen g ⇒ g → [FightDB] → [Winners] → [Int]
getBonusWinners' _ [] _ = []
getBonusWinners' gen (fh:ft) winners = excluded
  where
    candidates =
      safeShuffle curGen $
      map fst $ filter isCandidate $ HS.toList $ _dwfValue fh
    excluded =
      if null winners
        then candidates
        else filter (`notElem` _dwfValue wh) candidates ++
             getBonusWinners' newGen ft wt
    (wh:wt) = winners
    (curGen, newGen) = split gen

isCandidate ∷ (Int, FightData) → Bool
isCandidate (_, fightData) = all ((== 1) . fst) fightData

safeShuffle ∷ RandomGen g ⇒ g → [a] → [a]
safeShuffle _ [] = []
safeShuffle g as = shuffle' as (length as) g
