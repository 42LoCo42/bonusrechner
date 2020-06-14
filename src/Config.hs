{-# LANGUAGE UnicodeSyntax #-}

module Config where

import           DataWithFile
import           System.Random       (newStdGen)
import           Types

import qualified Data.HashMap.Strict as HS

mkInitialWorld ∷ IO World
mkInitialWorld = World (dataWithFile HS.empty "Namen.dof") [] [] <$> newStdGen

warLength ∷ Int
warLength = 7

dbDir ∷ FilePath
dbDir = "Datenbanken"

defaultWinnerCount ∷ Int
defaultWinnerCount = 8

emptyFightData ∷ FightData
emptyFightData = replicate 7 (-1, -1)
