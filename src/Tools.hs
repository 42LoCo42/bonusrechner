{-# LANGUAGE UnicodeSyntax #-}

module Tools where

import           Config
import           DataWithFile
import           HashMap                   ()
import           Menus
import           Types

import           Data.Char                 (toLower)
import qualified Data.HashMap.Strict       as HS
import           Lens.Micro                (ix, (%~), (&), (.~), (^.))
import           System.Console.ANSI.Codes
import           System.Directory          (doesFileExist)
import           Text.Printf               (printf)

-- Menu addons
reset00 ∷ String
reset00 = clearScreenCode ++ setCursorPositionCode 0 0

header ∷ MW → MW
header m@(MenuIO _) = m
header (Menu text action) = Menu text' action
  where
    text' world =
      reset00 ++
      printf
        "Clash of Clans - Bonusrechner\nAktueller Krieg: %d\n"
        (length $ _fightDBs world) ++
      text world

printAllPersons ∷ MW → MW
printAllPersons m@(MenuIO _) = m
printAllPersons (Menu text action) = Menu text' action
  where
    text' world =
      "Alle Personen:\n" ++
      unlines (map (uncurry $ printf "  %d: %s") (persons world)) ++
      text world
    persons world = HS.toList $ world ^. nameDB . dwfValue

showPersonAndFights ∷ String → FightData → String
showPersonAndFights name fights =
  printf "Daten für %s:\n" name ++ concatMap showFight ixdFights
  where
    showFight (day, (f, s))
      | f == -1 = printf "  Tag %d: Keine Daten eingetragen\n" day
      | otherwise = printf "  Tag %d: %d Kämpfe, %d Sterne\n" day f s
    ixdFights = zip [(1 :: Int) ..] fights

showPersonSummary ∷ String → FightData → String
showPersonSummary name fights =
  printf "  %s: %d Kämpfe, %d Sterne\n" name fightSum starSum
  where
    fightSum = foldr (\(f, _) fs -> max 0 f + fs) 0 fights
    starSum = foldr (\(_, s) ss -> max 0 s + ss) 0 fights

writeNameDB ∷ MW → MW
writeNameDB (MenuIO action) = MenuIO (writeData . _nameDB >> action)
writeNameDB m@(Menu _ _) = MenuIO saveAction
  where
    saveAction world = do
      writeData (_nameDB world)
      return $ NewMenu m

writeFightDB ∷ MW → MW
writeFightDB (MenuIO action) =
  MenuIO ((\w -> writeData $ head $ w ^. fightDBs) >> action)
writeFightDB m@(Menu _ _) = MenuIO saveAction
  where
    saveAction world = do
      writeData $ head $ world ^. fightDBs
      return $ NewMenu m

-- Database helper functions
findSimilarName ∷ NameDB → String → Maybe String
findSimilarName db name =
  (\nameID -> HS.lookupDefault "" nameID $ _dwfValue db) <$>
  HS.lookup (map toLower name) reverseDB
  where
    reverseDB =
      HS.fromList $ map (\(k, v) -> (map toLower v, k)) $ HS.toList $
      _dwfValue db

getIxForNewName ∷ NameDB → Int
getIxForNewName = (+ 1) . HS.size . _dwfValue

firstEmpty ∷ World → Int → Int
firstEmpty world pIx = minDay $ countFirstP fights (/= (-1, -1)) + 1
  where
    fights = world ^. fightDBs . ix 0 . dwfValue . ix pIx
    countFirstP [] _ = 0
    countFirstP (h:t) f
      | f h = 1 + countFirstP t f
      | otherwise = 0

-- World changer
addName ∷ String → World → World
addName name world =
  world & nameDB . dwfValue %~ HS.insert (getIxForNewName $ _nameDB world) name

readDBs ∷ World → IO World
readDBs world = do
  let num = show $ length (_fightDBs world)
      file = num ++ ".dof"
  exists <- doesFileExist file
  if not exists
    then return world
    else do
      let world1 = world & fightDBs %~ (dataWithFile HS.empty file :)
          winnerFile = num ++ "g.dof"
      winnerExists <- doesFileExist winnerFile
      if not winnerExists
        then return world1
        else do
          let world2 = world1 & winnersLists %~ (dataWithFile [] winnerFile :)
          readDBs world2

changeFightData ∷ Int → Int → Int → Int → World → World
changeFightData pIx day fights stars world =
  world & fightDBs . ix 0 . dwfValue . ix pIx . ix (day - 1) .~ (fights, stars)

-- Other helpers
minDay ∷ Int → Int
minDay = min warLength

delAt ∷ Int → [a] → [a]
delAt _ []    = []
delAt 0 (_:t) = t
delAt i (h:t) = h : delAt (i - 1) t
