{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Bonus
import           Config
import           Convert
import           DataWithFile
import           HashMap             ()
import           Menus
import           Tools
import           Types

import qualified Data.HashMap.Strict as HS
import           Lens.Micro          (at, each, ix, traverseOf, (%~), (&), (^.))
import           System.Directory    (createDirectoryIfMissing,
                                      setCurrentDirectory)
import           System.Random       (split)
import           Text.Printf         (printf)
import           Text.Read           (readMaybe)

main ∷ IO ()
main = do
  createDirectoryIfMissing True dbDir
  setCurrentDirectory dbDir
  convertAll
  initialWorld <- mkInitialWorld
  createIfMissing $ _nameDB initialWorld
  createIfMissing (dataWithFile (HS.empty :: HS.HashMap Int [Int]) "0.dof")
  world1 <- traverseOf nameDB updateData initialWorld
  world2 <- readDBs world1
  world3 <- traverseOf (fightDBs . each) updateData world2
  world4 <- traverseOf (winnersLists . each) updateData world3
  _ <- runMenu mainMenu world4
  setCurrentDirectory ".."

mainMenu ∷ MW
mainMenu = header $ Menu text action
  where
    text _ =
      unlines
        [ "Hauptmenü"
        , "  1 Neue Person einfügen"
        , "  2 Daten ändern"
        , "  3 Bonus-Gewinner ermitteln"
        , "  4 Übersicht anzeigen"
        , "  5 Beenden"
        ]
    action line world =
      case (readMaybe line :: Maybe Int) of
        Nothing -> Just $ State world
        Just res ->
          case res of
            1 -> Just $ NewMenu addPerson
            2 -> Just $ NewMenu choosePersonToEdit
            3 ->
              Just $
              NewMenu (bonusWinners defaultWinnerCount $ getBonusWinners world)
            4 -> Just $ NewMenu summary
            5 -> Just $ State world
            _ -> Nothing

addPerson ∷ MW
addPerson = writeNameDB $ header $ printAllPersons $ Menu text action
  where
    text _ = "Neuen Namen eingeben: "
    action line world =
      case line of
        [] -> Just $ NewMenu mainMenu
        name ->
          case findSimilarName (_nameDB world) name of
            Nothing -> Just $ NewMenuAndState addPerson (addName name world)
            Just simName -> Just $ NewMenu $ useSimilarName name simName

useSimilarName ∷ String → String → MW
useSimilarName name simName = Menu text action
  where
    text _ = printf "Ist Name %s gemeint? (j/n) " simName
    action line world =
      case line of
        [] -> Nothing
        _
          | head line `elem` ['y', 'j'] -> Just $ NewMenu addPerson
          | head line == 'n' ->
            Just $ NewMenuAndState addPerson (addName name world)
          | otherwise -> Nothing

choosePersonToEdit ∷ MW
choosePersonToEdit = writeFightDB $ header $ printAllPersons $ Menu text action
  where
    text _ = "Person auswählen: "
    action line world =
      case line of
        [] -> Just $ NewMenu mainMenu
        _ ->
          case (readMaybe line :: Maybe Int) of
            Nothing -> Nothing
            Just pIx ->
              if pIx `notElem` [1 .. HS.size (world ^. nameDB . dwfValue)]
                then Nothing -- pIx out of range
                else Just $
                     NewMenuAndState
                       (editPerson pIx $ firstEmpty newWorld pIx)
                       newWorld
              where newWorld =
                      world & fightDBs . ix 0 . dwfValue . at pIx %~
                      insertDefault
    insertDefault Nothing    = Just emptyFightData
    insertDefault v@(Just _) = v

editPerson ∷ Int → Int → MW
editPerson pIx day = writeFightDB $ header $ Menu text action
  where
    name world = world ^. nameDB . dwfValue . ix pIx
    fightData world = world ^. fightDBs . ix 0 . dwfValue . ix pIx
    text world =
      showPersonAndFights (name world) (fightData world) ++
      printf "Ändere Tag %d\n" day ++
      "Anzahl Kämpfe (0-1) oder [a]nderen Tag wählen: "
    action line world =
      case line of
        [] -> Just $ NewMenu choosePersonToEdit
        "a" -> Just $ NewMenu (otherDay $ editPerson pIx)
        _ ->
          case (readMaybe line :: Maybe Int) of
            Nothing -> Nothing
            Just fights ->
              if fights == 0
                then Just $
                     NewMenuAndState
                       (editPerson pIx $ minDay (day + 1))
                       (changeFightData pIx day 0 0 world)
                else if fights == 1
                       then Just $ NewMenu $ editPerson2 pIx day fights
                       else Nothing

editPerson2 ∷ Int → Int → Int → MW
editPerson2 pIx day fights = Menu text action
  where
    text _ = "Anzahl Sterne (0-3): "
    action line world =
      case (readMaybe line :: Maybe Int) of
        Nothing -> Nothing
        Just stars ->
          if stars `elem` [0 .. 3]
            then Just $
                 NewMenuAndState
                   (editPerson pIx $ minDay (day + 1))
                   (changeFightData pIx day fights stars world)
            else Nothing

otherDay ∷ (Int → MW) → MW
otherDay menu = Menu text action
  where
    text _ = "Wähle Tag (1-7): "
    action line _ =
      case (readMaybe line :: Maybe Int) of
        Nothing -> Nothing
        Just day ->
          if day `elem` [1 .. warLength]
            then Just $ NewMenu $ menu day
            else Nothing

bonusWinners ∷ Int → [Int] → MW
bonusWinners winnerCount winnerIDs = header $ Menu text action
  where
    (wh, wt) = splitAt winnerCount winnerIDs
    text world =
      "Bonus-Gewinner:\n" ++
      concat (zipWith (printf "  %d: %s\n") [(1 :: Int) ..] winnerNames) ++
      printf "Noch %d Alternativen\n" (length wt) ++
      "Abwahl oder [s]peichern\noder [a]ndere Anzahl oder [e]rneut würfeln: "
      where
        winnerNames =
          map (\k -> HS.lookupDefault "" k $ world ^. nameDB . dwfValue) wh
    action line world =
      case line of
        [] -> Just $ NewMenu mainMenu
        "s" -> Just $ NewMenu (saveBonusWinners wh)
        "a" ->
          Just $ NewMenu (chooseBonusWinnerCount (`bonusWinners` winnerIDs))
        "e" ->
          Just $
          NewMenuAndState (bonusWinners winnerCount newWinnerIDs) newWorld
          where newWorld = world & stdGen %~ (fst . split)
                newWinnerIDs = getBonusWinners newWorld
        _ ->
          case (readMaybe line :: Maybe Int) of
            Nothing -> Nothing
            Just deselect ->
              if deselect `notElem` [1 .. (min winnerCount $ length winnerIDs)]
                then Nothing
                else Just $
                     NewMenu
                       (bonusWinners winnerCount $
                        delAt (deselect - 1) winnerIDs)

chooseBonusWinnerCount ∷ (Int → MW) → MW
chooseBonusWinnerCount menu = Menu text action
  where
    text _ = "Anzahl Gewinner: "
    action line _ =
      case (readMaybe line :: Maybe Int) of
        Nothing          -> Nothing
        Just winnerCount -> Just $ NewMenu (menu winnerCount)

summary ∷ MW
summary = header $ Menu text action
  where
    text world =
      "Übersicht\n" ++
      concatMap (\(pIx, name) -> showPersonSummary name $ fightData pIx) names
      where
        names = HS.toList $ world ^. nameDB . dwfValue
        fights = world ^. fightDBs . ix 0 . dwfValue
        fightData pIx = HS.lookupDefault emptyFightData pIx fights
    action _ _ = Just $ NewMenu mainMenu

saveBonusWinners ∷ [Int] → MW
saveBonusWinners winnerIDs = MenuIO action
  where
    action world0 = do
      let winnerFileName = show (length $ world0 ^. winnersLists) ++ "g.dof"
          fightsFileName = show (length $ world0 ^. fightDBs) ++ ".dof"
          winnerFile = dataWithFile winnerIDs winnerFileName
          fightsFile = dataWithFile HS.empty fightsFileName :: FightDB
          world1 = world0 & winnersLists %~ (winnerFile :)
          world2 = world1 & fightDBs %~ (fightsFile :)
      writeData winnerFile
      writeData fightsFile
      return $ NewMenuAndState mainMenu world2
