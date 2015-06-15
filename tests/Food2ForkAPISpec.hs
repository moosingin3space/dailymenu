{-# LANGUAGE OverloadedStrings #-}
module Food2ForkAPISpec where

import SpecHelper
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding

prop_recipeToRecipeIdWorks :: Recipe -> Bool
prop_recipeToRecipeIdWorks recipe = 
    (encodeUtf8 $ recipe_id recipe) == (toRecipeId recipe)

prop_textToRecipeIdWorks :: T.Text -> Bool
prop_textToRecipeIdWorks txt = (encodeUtf8 txt) == (toRecipeId txt)

prop_byteStringToRecipeIdWorks :: B.ByteString -> Bool
prop_byteStringToRecipeIdWorks bs = (id bs) == (toRecipeId bs)

prop_searchUrlIsValid :: B.ByteString -> B.ByteString -> SortOrder -> Int -> Bool
prop_searchUrlIsValid apiKey searchQuery sortOrder pageNumber = 
    let url = mkSearchUrl apiKey searchQuery sortOrder pageNumber 
     in (Just apiKey == apiKeyFromUrl url 
      && Just searchQuery == searchQueryFromUrl url
      && Just (B.pack $ show sortOrder) == sortOrderFromUrl url
      && Just (intToByteString pageNumber) == pageNumberFromUrl url)

prop_getUrlIsValid :: B.ByteString -> B.ByteString -> Bool
prop_getUrlIsValid apiKey rId = 
    let url = mkGetUrl apiKey rId
     in (Just apiKey == apiKeyFromUrl url
      && Just rId == recipeIdFromUrl url)

spec :: Spec
spec = do
    describe "The URL-generating functions" $ do
        it "generate expected search URLs" $ property $
          prop_searchUrlIsValid
        it "generate expected get URLs" $ property $
          prop_getUrlIsValid
    describe "The ToRecipeId typeclass instances" $ do
        it "work properly for a Recipe" $ property $
          prop_recipeToRecipeIdWorks
        it "work properly for a Text" $ property $
          prop_textToRecipeIdWorks
        it "work properly for a ByteString" $ property $
          prop_byteStringToRecipeIdWorks

main :: IO ()
main = hspec spec
