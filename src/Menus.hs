{-# LANGUAGE UnicodeSyntax #-}

module Menus where

import Control.Monad.Fix (fix)

data Menu s
  = Menu (s → String) (String → s → Maybe (MenuResult s))
  | MenuIO (s → IO (MenuResult s))

data MenuResult s
  = State s
  | NewMenu (Menu s)
  | NewMenuAndState (Menu s) s

runMenu ∷ Menu s → s → IO s
runMenu menu state =
  case menu of
    Menu text action -> do
      putStr $ text state
      fix $ \loop -> do
        line <- getLine
        let resultM = action line state
        case resultM of
          Nothing -> do
            putStrLn "Fehlerhafte Eingabe!"
            loop
          Just result -> handleResult result
    MenuIO action -> action state >>= handleResult
  where
    handleResult result =
      case result of
        State s                      -> return s
        NewMenu menu'                -> runMenu menu' state
        NewMenuAndState menu' state' -> runMenu menu' state'
