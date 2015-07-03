{-# LANGUAGE OverloadedStrings #-}
module Storage where

import qualified Food2ForkAPI as F2F
import Data.Aeson.Encode
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified System.Directory as Dir

rootDirectory :: IO FilePath
rootDirectory = Dir.getAppDataDirectory "dailymenu"

underscoreize :: T.Text -> T.Text
underscoreize = T.replace " " "_"

getFileName :: F2F.Recipe -> IO FilePath
getFileName recipe = do
    root <- rootDirectory
    return $ addExtension (root </> (underscoreize $ F2F.title recipe)) "json"

saveRecipe :: F2F.Recipe -> IO ()
saveRecipe recipe = do
    fileName <- getFileName recipe
    let contents = encode recipe
    BL.writeFile (getFileName recipe) contents
