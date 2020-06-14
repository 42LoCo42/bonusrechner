{-# LANGUAGE TemplateHaskell #-}

module DataWithFile where

import Control.Monad (unless)
import Lens.Micro.TH (makeLenses)
import System.Directory (doesFileExist)

data DataWithFile a =
  DataWithFile
    { _dwfValue :: a
    , _dwfFile :: String
    }
  deriving (Show)

makeLenses ''DataWithFile

-- Functions
dataWithFile :: (Read a, Show a) => a -> String -> DataWithFile a
dataWithFile = DataWithFile

writeData :: (Read a, Show a) => DataWithFile a -> IO ()
writeData (DataWithFile a file) = writeFile file $ show a

readData :: (Read a, Show a) => FilePath -> IO (DataWithFile a)
readData file = do
  appendFile file ""
  a <- read <$> readFile file
  seq a $ return $ DataWithFile a file

updateData :: (Read a, Show a) => DataWithFile a -> IO (DataWithFile a)
updateData (DataWithFile _ file) = readData file

createIfMissing :: (Read a, Show a) => DataWithFile a -> IO ()
createIfMissing (DataWithFile val file) = do
  exists <- doesFileExist file
  unless exists $ writeFile file $ show val

-- Instances
instance Functor DataWithFile where
  fmap f dwf = dwf {_dwfValue = f $ _dwfValue dwf}
