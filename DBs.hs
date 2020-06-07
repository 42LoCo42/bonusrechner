{-# LANGUAGE UnicodeSyntax #-}

module DBs where

import CSV
import Config

import System.Directory

loadAll :: IO [DB]
loadAll = do
  nameDB <- loadDB nameDBFile
  fightDBs <- loadFightDB 0 False
  return (nameDB : reverse fightDBs)

loadFightDB :: Int -> Bool -> IO [DB]
loadFightDB ix isWin = do
  let file =
        show ix ++
        (if isWin
           then "g.csv"
           else ".csv")
  exists <- doesFileExist file
  if exists
    then do
      let nextIx =
            if isWin
              then ix + 1
              else ix
          nextIsWin = not isWin
      nextDBs <- loadFightDB nextIx nextIsWin
      thisDB <- loadDB file
      return (thisDB : nextDBs)
    else return []
