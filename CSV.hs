module CSV
  ( module CSV
  , module Text.CSV
  ) where

import           Data.Either
import qualified Data.HashMap.Strict as HS
import           Data.List           (intercalate)
import           Text.CSV
import           Text.Parsec.Error

type DB = HS.HashMap String [String]

showRecord :: Record -> String
showRecord = intercalate "," . map (\s -> '"' : s ++ "\"")

showCSV :: CSV -> String
showCSV = concatMap (\r -> showRecord r ++ "\n")

recordToKV :: Record -> (String, [String])
recordToKV (h:t) = (h,t)
recordToKV _     = ("",[])

csvToDB :: CSV -> DB
csvToDB = foldr (uncurry HS.insert . recordToKV) HS.empty

dbToCSV :: DB -> CSV
dbToCSV = map toRecord . HS.toList
  where
    toRecord (_, []) = []
    toRecord (k, vs) = k:vs

readDBFile :: FilePath -> IO (Either ParseError DB)
readDBFile file = do
  parseResult <- fixRecords <$> parseCSVFromFile file
  return (csvToDB <$> parseResult)

fixRecords :: Either ParseError CSV -> Either ParseError CSV
fixRecords rawData = do
  records <- rawData
  let realLength = length records - 1
  return $ take realLength records

loadDB :: FilePath -> IO DB
loadDB file = do
  result <- readDBFile file
  if isLeft result then
    error $ show result
  else
    return $ fromRight HS.empty result
