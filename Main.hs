module Main where

import qualified Data.HashMap.Strict as HS
import Data.Either
import System.IO

import CSV

main :: IO ()
main = do
  parseResult <- parseCSVFromFile "personen.csv"
  if isLeft parseResult then do
    putStrLn "Datenbank kann nicht gelesen werden!"
    print parseResult
  else do
    let database = fromRight [] parseResult
        hashmap  = csvToHS database
    input <- newPerson
    print $ input : database
    print hashmap
    
newPerson :: IO Record
newPerson = do
  putStr "Name: "
  name <- getLine
  putStr "Kämpfe (0, 1 oder 2): "
  fights <- checkedInput read (\a -> a >= 0 && a <= 2) :: IO Int
  putStr "Sterne (0 bis 6): "
  stars <- checkedInput read (\a -> a >= 0 && a <= 6) :: IO Int
  return [name, show fights, show stars]

csvToHS :: CSV -> HS.HashMap String [String]
csvToHS = foldr (uncurry HS.insert) HS.empty . map recordToKV

checkedInput :: (String -> a) -> (a -> Bool) -> IO a
checkedInput parser checker = do
  line <- getLine
  let val = parser line
  if checker val
    then return val
    else checkedInput parser checker
