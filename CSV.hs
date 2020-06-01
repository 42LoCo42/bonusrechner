module CSV
  ( module CSV
  , module Text.CSV
  ) where
  
import Text.CSV
import Data.List (intersperse)

showRecord :: Record -> String
showRecord = concat . intersperse "," . map (\s -> '"' : s ++ "\"")

showCSV :: CSV -> String
showCSV = concat . intersperse "\n" . map showRecord

recordToKV :: Record -> (String, [String])
recordToKV (h:t) = (h,t)
recordToKV _     = ("",[])
