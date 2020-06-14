{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Types where

import           DataWithFile
import           Menus               (Menu)

import qualified Data.HashMap.Strict as HS
import           Lens.Micro.TH       (makeLenses)
import           System.Random       (StdGen)

-- Types
type FightData = [(Int, Int)] -- fights, stars

type NameDB = DataWithFile (HS.HashMap Int String)

type FightDB = DataWithFile (HS.HashMap Int FightData)

type Winners = DataWithFile [Int]

type MW = Menu World

-- Data
data World =
  World
    { _nameDB       :: NameDB
    , _fightDBs     :: [FightDB]
    , _winnersLists :: [Winners]
    , _stdGen       :: StdGen
    }
  deriving (Show)

makeLenses ''World
