{-# LANGUAGE UnicodeSyntax #-}

module Convert where

import           CSV
import           DataWithFile
import           Types

import           Control.Monad       (when)
import qualified Data.HashMap.Strict as HS
import           System.Directory

convertAll ∷ IO ()
convertAll = do
  contents <- listDirectory "."
  mapM_ handleFile contents

handleFile ∷ FilePath → IO ()
handleFile file =
  when ('c' `elem` file) $ do
    db <- HS.toList <$> loadDB file
    case file of
      "Namen.csv" -> do
        let newDB = map (\(pIx, (name:_)) -> (read pIx :: Int, name)) db
            newName = "Namen.dof"
        writeData $ dataWithFile (HS.fromList newDB) newName
      (n:"g.csv") -> do
        let newDB = foldr (\(pIx, _) ids -> (read pIx :: Int) : ids) [] db
            newName = n : "g.dof"
        writeData $ dataWithFile newDB newName
      (n:".csv") -> do
        let newDB =
              map
                (\(pIx, fights) ->
                   (read pIx :: Int, map read fights :: FightData))
                db
            newName = n : ".dof"
        writeData $ dataWithFile (HS.fromList newDB) newName
    removeFile file
