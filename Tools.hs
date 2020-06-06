module Tools where

import           Config
import           CSV

import           Data.Char
import qualified Data.HashMap.Strict as HS
import           Data.Maybe
import           System.Console.ANSI
import           Text.Printf         (printf)
import           Text.Read           (readMaybe)

checkedInput :: Read a => (a -> Bool) -> IO a
checkedInput check = do
  let retry = putStrLn "Fehlerhafte Eingabe!" >> checkedInput check
  res <- readMaybe <$> getLine
  case res of
    Just a  -> if check a then return a else retry
    Nothing -> retry

checkedInputFull :: (String -> a) -> (a -> Bool) -> IO a
checkedInputFull parse check = do
  let retry = putStrLn "Fehlerhafte Eingabe!" >> checkedInputFull parse check
  res <- parse <$> getLine
  if check res then
    return res
  else
    retry

getRoundStatus :: [DB] -> (Int, Bool)
getRoundStatus dbs = ((length dbs + 1) `div` 2, odd $ length dbs)

showMenu :: [DB] -> ([DB] -> IO ()) -> IO ()
showMenu dbs menu = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Clash of Clans - Bonusrechner"
  let (roundN, isWin) = getRoundStatus dbs
      prefix          = if isWin then "Neue" else "Aktuelle"
  putStrLn $ printf "%s Runde: %d" prefix roundN
  menu dbs

nameCheck :: DB -> String -> Maybe (String, String)
nameCheck _      ""   = Nothing
nameCheck nameDB name = case found of
  Just k  -> Just (k, fromJust realName)
  Nothing -> Nothing
  where
    -- lowercase name to key
    inverted = HS.fromList $ map invertNameKV $ HS.toList nameDB
    -- are either both Nothing or Just
    found    = head <$> HS.lookup (map toLower name) inverted -- maybe key
    realName = head . fromJust . (`HS.lookup` nameDB) <$> found -- maybe real name of key

invertNameKV :: (String, [String]) -> (String, [String])
invertNameKV (_, [])       = ("", [])
invertNameKV (k, (name:_)) = (map toLower name, [k])

findHighestID :: DB -> Int
findHighestID = maximum . (0:) . map read . HS.keys

addNewName :: String -> DB -> IO ()
addNewName name db = do
  let nameID = findHighestID db + 1
      db'    = HS.insert (show nameID) [name] db
  writeFile nameDBFile $ showCSV $ dbToCSV db'
  return ()

yesNoChoice :: IO String
yesNoChoice = checkedInputFull id (\c -> not (null c) &&
  head c `elem` ['j', 'y', 'n'])

fightDataText :: (Int, String) -> String
fightDataText (ix, raw)
  | fights == -1 = printf "  Tag %d: Keine Daten eingetragen\n" ix
  | otherwise   = printf "  Tag %d: %d Kämpfe %d Sterne\n" ix fights stars
  where (fights, stars) = read raw :: (Int, Int)

bonusWinnerText :: DB -> DB -> (Int, String) -> String
bonusWinnerText nameDB currentFightDB (ix, nameID)
  | null fightData
  = printf "  %d: %s (Keine Daten in dieser Runde)\n" ix name
  | otherwise
  = printf "  %d: %s (%d Kämpfe, %d Sterne)\n" ix name sumFights sumStars
  where
    name                  = head $ fromJust $ HS.lookup nameID nameDB
    fightData             = HS.lookupDefault [] nameID currentFightDB
    parsedFightData       = map read fightData :: [(Int, Int)]
    (sumFights, sumStars) = foldr
      (\(f1, s1) (f2, s2) -> (f2 + max f1 0, s2 + max s1 0)) -- don't count -1
      (0, 0) parsedFightData

setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 a (_:t) = a:t
setAt i a (h:t)
  | i < 0     = []
  | otherwise = h : setAt (i-1) a t

removeAt :: Int -> [a] -> [a]
removeAt _  []    = []
removeAt 0  (_:t) = t
removeAt ix (h:t) = h : removeAt (ix-1) t
