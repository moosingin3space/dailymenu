{-# LANGUAGE OverloadedStrings #-}
module Food2ForkAPISpec where

import SpecHelper
import qualified Data.ByteString.Char8 as B

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

main :: IO ()
main = hspec spec
