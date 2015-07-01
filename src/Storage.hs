{-# LANGUAGE OverloadedStrings #-}
module Storage where

import qualified Food2ForkAPI as F2F
import Data.Aeson
import System.FilePath
import qualified Data.Text as T
import qualified System.Directory as Dir

rootDirectory :: IO FilePath
rootDirectory = Dir.getAppDataDirectory "dailymenu"

getFileName :: F2F.Recipe -> IO FilePath
getFileName recipe = do
    root <- rootDirectory
    return $ addExtension (root </> (F2F.title recipe)) "json"

-- TODO implement query functions
