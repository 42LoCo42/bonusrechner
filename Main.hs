{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Bonus
import           Config
import           CSV
import           DBs
import           Tools

import           Control.Monad       (when)
import qualified Data.HashMap.Strict as HS
import           Data.List           (sortOn)
import           Data.Maybe          (fromJust)
import           System.Directory    (createDirectoryIfMissing,
                                      setCurrentDirectory)
import           System.IO           (BufferMode (NoBuffering), hSetBuffering,
                                      stdout)
import           System.Random       (newStdGen)
import           Text.Printf         (printf)

main ∷ IO ()
main = do
  createDirectoryIfMissing True dbFolder
  setCurrentDirectory dbFolder
  appendFile nameDBFile ""
  hSetBuffering stdout NoBuffering
  start
  setCurrentDirectory ".."

start ∷ IO ()
start = do
  dbs <- loadAll
  showMenu dbs roundCheckMenu

roundCheckMenu ∷ [DB] → IO ()
roundCheckMenu dbs = do
  let (roundN, isWin) = getRoundStatus dbs
  if isWin
    then do
      putStr $ printf "Krieg starten? j/n "
      choice <- yesNoChoice
      when (choice == "j" || choice == "y") $ do
        appendFile (printf "%d.csv" roundN) ""
        start
    else mainMenu dbs

mainMenu ∷ [DB] → IO ()
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

printAllPersons ∷ DB → IO ()
printAllPersons dbs = do
  putStrLn "Alle Personen: "
  mapM_ (\(k, (v:_)) -> printf "  %s %s\n" k v) $
    sortOn ((read :: String → Int) . fst) $ HS.toList dbs

newPersonMenu ∷ [DB] → IO ()
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
      dbs' <- loadAll -- TODO: don't reload every time, use modified db from addNewName?
      showMenu dbs' newPersonMenu

choosePersonMenu ∷ [DB] → IO ()
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

modifyPersonMenu ∷ (String, String) → [DB] → IO ()
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
      newDay = countRealData record + 1
      -- show data of all days
      showAllDays = mapM_ (putStr . fightDataText) (zip [1 ..] record)
      -- read fights and stars
      modifyDayMenu ch = do
        putStr $ printf "Ändere Tag %d\n" ch
        putStr "Anzahl Kämpfe (0-1) oder [A]nderen Tag wählen: "
        fights <-
          checkedInputFull
            id
            (\c -> null c || c == "a" || (read c :: Int) `elem` [0, 1])
        case fights of
          []  -> start
          "a" -> chooseDayMenu
          _   -> modifyDayMenu2 ch (read fights :: Int)
      -- get fights, read stars, write both to selected day
      modifyDayMenu2 ch fights = do
        stars <-
          if fights == 0
            then return 0
            else do
              putStr "Anzahl Sterne (0-3): "
              checkedInput (`elem` [0 .. 3]) :: IO Int
        let newVal = show (fights, stars)
            record' = setAt (ch - 1) newVal record
            currentFightDB'' = HS.insert nameID record' currentFightDB'
            dbs' = setAt 1 currentFightDB'' dbs
            (dbIx, _) = getRoundStatus dbs'
            dbName = show dbIx ++ ".csv"
        writeFile dbName $ showCSV $ dbToCSV currentFightDB''
        showMenu dbs' (modifyPersonMenu p)
      -- read one day and show modifyDayMenu
      chooseDayMenu = do
        putStrLn "Tag auswählen (1-7): "
        choice <-
          checkedInputFull id (\c -> null c || (read c :: Int) `elem` [1 .. 7])
        if null choice
          then start
          else modifyDayMenu (read choice :: Int)
  putStrLn $ printf "Daten für \"%s\" ändern" name
  showAllDays
  modifyDayMenu newDay

--        fights <- checkedInput (`elem` [0 .. 1]) :: IO Int
getBonusMenu ∷ [DB] → IO ()
getBonusMenu dbs = do
  gen <- newStdGen
  let (_:fightDBs) = dbs
      winners = getBonusWinners gen fightDBs
  showMenu dbs (changeBonusMenu winners)

changeBonusMenu ∷ [String] → [DB] → IO ()
changeBonusMenu ids dbs = do
  let (nameDB:currentFightDB:_) = dbs
      (chosen, rest) = splitAt 8 ids
  mapM_
    (putStr . bonusWinnerText nameDB currentFightDB)
    (zip ([1 ..] :: [Int]) chosen)
  putStr $ printf "Noch %d Alternativen\n" (length rest)
  putStr "Enter, [s]peichern oder Abwahl: "
  choice <-
    checkedInputFull
      id
      (\c -> null c || c == "s" || (read c :: Int) `elem` [1 .. length chosen])
  case choice of
    [] -> start
    "s" -> do
      let curRound = fst $ getRoundStatus dbs
      saveBonus chosen $ show curRound ++ "g.csv"
    _ -> do
      let removeIx = read choice - 1 :: Int
          newIds = removeAt removeIx ids
      showMenu dbs (changeBonusMenu newIds)

saveBonus ∷ [String] → String → IO ()
saveBonus ids file = do
  writeFile file $ unlines ids
  start
