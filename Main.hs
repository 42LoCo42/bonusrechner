{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Bonus
import CSV
import Config
import DBs
import Tools

import Control.Monad (when)
import qualified Data.HashMap.Strict as HS
import Data.List (sortOn)
import Data.Maybe (fromJust)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (newStdGen)
import Text.Printf (printf)

main :: IO ()
main = do
  createDirectoryIfMissing True dbFolder
  setCurrentDirectory dbFolder
  appendFile nameDBFile ""
  hSetBuffering stdout NoBuffering
  start
  setCurrentDirectory ".."

start :: IO ()
start = do
  dbs <- loadAll
  showMenu dbs roundCheckMenu

roundCheckMenu :: [DB] -> IO ()
roundCheckMenu dbs = do
  let (roundN, isWin) = getRoundStatus dbs
  if isWin
    then do
      putStr $ printf "Runde starten? j/n "
      choice <- yesNoChoice
      when (choice == "j" || choice == "y") $ do
        appendFile (printf "%d.csv" roundN) ""
        start
    else mainMenu dbs

mainMenu :: [DB] -> IO ()
mainMenu dbs = do
  putStrLn "Hauptmenü"
  putStrLn "  1 Neue Person einfügen"
  putStrLn "  2 Daten ändern"
  putStrLn "  3 Bonus-Gewinner ermitteln"
  putStrLn "  4 Beenden"
  choice <- checkedInput (`elem` [1 .. 4]) :: IO Int
  case choice of
    1 -> showMenu dbs newPersonMenu
    2 -> showMenu dbs choosePersonMenu
    3 -> showMenu dbs getBonusMenu
    4 -> return ()
    _ -> undefined

printAllPersons :: DB -> IO ()
printAllPersons dbs = do
  putStrLn "Alle Personen: "
  mapM_ (\(k, (v:_)) -> printf "  %s %s\n" k v) $
    sortOn ((read :: String -> Int) . fst) $ HS.toList dbs

newPersonMenu :: [DB] -> IO ()
newPersonMenu dbs = do
  let (nameDB:_) = dbs
  printAllPersons nameDB
  putStr "Neuen Namen eingeben: "
  name <- getLine
  if null name
    then start
    else do
      let realNameM = nameCheck nameDB name
      case realNameM of
        Just (_, realName) -- name exists
         ->
          when (name /= realName) $ -- ... but was entered differently
           do
            putStrLn $ printf "Ist Name \"%s\" gemeint? j/n" realName
            choice <- head <$> yesNoChoice
            when (choice == 'n') $ addNewName name nameDB
        Nothing -- add new id and name
         -> addNewName name nameDB
      dbs' <- loadAll
      showMenu dbs' newPersonMenu

choosePersonMenu :: [DB] -> IO ()
choosePersonMenu dbs = do
  let (nameDB:_) = dbs
  printAllPersons nameDB
  putStr "Auswahl: "
  choice <-
    checkedInputFull id (\c -> null c || c `elem` map fst (HS.toList nameDB))
  if null choice
    then start
    else do
      let person = (choice, head . fromJust $ HS.lookup choice nameDB)
      showMenu dbs (modifyPersonMenu person)

modifyPersonMenu :: (String, String) -> [DB] -> IO ()
modifyPersonMenu p@(nameID, name) dbs = do
  let (_:fightDBs) = dbs
      currentFightDB = head fightDBs -- most recent fight is head
      recordM = HS.lookup nameID currentFightDB
      (record, currentFightDB') =
        case recordM -- add empty entry when not found
              of
          Just r -> (r, currentFightDB)
          Nothing -> (newRecord, HS.insert nameID newRecord currentFightDB)
            where newRecord = replicate 7 "(-1, -1)"
  putStrLn $ printf "Daten für \"%s\" ändern" name
  mapM_ (putStr . fightDataText) (zip [1 ..] record)
  choice <-
    checkedInputFull id (\c -> null c || (read c :: Int) `elem` [1 .. 7])
  if null choice
    then start
    else do
      putStr "Anzahl Kämpfe (0-1): "
      fights <- checkedInput (`elem` [0 .. 1]) :: IO Int
      putStr "Anzahl Sterne (0-3): "
      stars <- checkedInput (`elem` [0 .. 3]) :: IO Int
      let newVal = show (fights, stars)
          record' = setAt (read choice - 1) newVal record
          currentFightDB'' = HS.insert nameID record' currentFightDB'
          dbs' = setAt 1 currentFightDB'' dbs
          (dbIx, _) = getRoundStatus dbs'
          dbName = show dbIx ++ ".csv"
      writeFile dbName $ showCSV $ dbToCSV currentFightDB''
      showMenu dbs' (modifyPersonMenu p)

getBonusMenu :: [DB] -> IO ()
getBonusMenu dbs = do
  gen <- newStdGen
  let (_:fightDBs) = dbs
      winners = getBonusWinners gen fightDBs
  showMenu dbs (changeBonusMenu winners)

changeBonusMenu :: [String] -> [DB] -> IO ()
changeBonusMenu ids dbs = do
  let (nameDB:currentFightDB:_) = dbs
      (chosen, rest) = splitAt 8 ids
  mapM_
    (putStr . bonusWinnerText nameDB currentFightDB)
    (zip ([1 ..] :: [Int]) chosen)
  putStrLn $ printf "Noch %d Alternativen\nEnter oder Abwahl: " (length rest)
  choice <-
    checkedInputFull
      id
      (\c -> null c || (read c :: Int) `elem` [1 .. length chosen])
  if null choice
    then do
      let curRound = fst $ getRoundStatus dbs
      saveBonus chosen $ show curRound ++ "g.csv"
    else do
      let removeIx = read choice - 1 :: Int
          newIds = removeAt removeIx ids
      showMenu dbs (changeBonusMenu newIds)

saveBonus :: [String] -> String -> IO ()
saveBonus ids file = do
  writeFile file $ unlines ids
  start
