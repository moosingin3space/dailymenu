{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Food2ForkAPISpec where

import SpecHelper
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe

httpGetter :: B.ByteString -> IO (Maybe BL.ByteString)
httpGetter url
    | searchEndpoint `B.isPrefixOf` url = return $ Just [here|
        {
            "count": 1,
            "recipes": [{
                      "publisher": "Allrecipes.com",
                      "social_rank": 99.81007979198002, 
                      "f2f_url": "http://food2fork.com/F2F/recipes/view/29159",
                      "publisher_url": "http://allrecipes.com",
                      "title": "Slow-Cooker Chicken Tortilla Soup", 
                      "source_url": "http://allrecipes.com/Recipe/Slow-Cooker-Chicken-Tortilla-Soup/Detail.aspx",
                      "page":1}]
        }
    |]
    | getRecipeEndpoint `B.isPrefixOf` url = return $ Just [here|
        {
            "recipe": {
                "publisher": "Real Simple",
                "f2f_url": "http://food2fork.com/view/37859",
                "ingredients": [
                    "1 1/2 cups shredded rotisserie chicken",
                    "1 1/2 cups grated Gruyre",
                    "1 cup frozen peas",
                    "2 sheets (one 17.25-ounce package) frozen puff pastry, thawed",
                    "1 large egg, beaten",
                    "1/4 cup Dijon mustard\n"
                ],
                "source_url": "http://www.realsimple.com/food-recipes/browse-all-recipes/chicken-and-gruyere-turnovers-00000000052482/index.html",
                "recipe_id": "37859",
                "image_url": "http://static.food2fork.com/chickenturnover2_300e6667e66.jpg",
                "social_rank": 99.84842829206659,
                "publisher_url": "http://realsimple.com",
                "title": "Chicken and Gruyre Turnovers"
            }
        }
    |]
    | otherwise = return Nothing

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

prop_searchUrlIncludesSearchEndpoint :: B.ByteString -> B.ByteString -> SortOrder -> Int -> Bool
prop_searchUrlIncludesSearchEndpoint apiKey searchQuery sortOrder pageNumber =
    searchEndpoint `B.isPrefixOf` (mkSearchUrl apiKey searchQuery sortOrder pageNumber)

prop_getUrlIncludesGetEndpoint :: B.ByteString -> B.ByteString -> Bool
prop_getUrlIncludesGetEndpoint apiKey rId =
    getRecipeEndpoint `B.isPrefixOf` (mkGetUrl apiKey rId)

getDummyRecipe :: IO (Maybe Recipe)
getDummyRecipe = getRecipe httpGetter dummyKey dummyId
    where dummyKey = "dummy" :: B.ByteString
          dummyId = "dummy" :: B.ByteString

spec :: Spec
spec = do
    describe "The search URL-generating function" $ do
        it "generates URLs with the correct query parameters" $ property $
          prop_searchUrlIsValid
        it "generates URLs that start with the search endpoint" $ property $
          prop_searchUrlIncludesSearchEndpoint
    describe "The get URL-generating function" $ do
        it "generates URLs with the correct query parameters" $ property $
          prop_getUrlIsValid
        it "generates URLs that start with the get endpoint" $ property $
          prop_getUrlIncludesGetEndpoint
    describe "The ToRecipeId typeclass instances" $ do
        it "work properly for a Recipe" $ property $
          prop_recipeToRecipeIdWorks
        it "work properly for a Text" $ property $
          prop_textToRecipeIdWorks
        it "work properly for a ByteString" $ property $
          prop_byteStringToRecipeIdWorks
    describe "The getRecipe function, when returning proper data" $ do 
        it "will not be Nothing" $ do
            recipe <- getDummyRecipe
            recipe `shouldSatisfy` isJust
        it "will return the correct image url" $ do
            recipe <- getDummyRecipe
            (fmap image_url recipe) `shouldBe` Just "http://static.food2fork.com/chickenturnover2_300e6667e66.jpg"
        it "will return the correct recipe ID" $ do
            recipe <- getDummyRecipe
            (fmap recipe_id recipe) `shouldBe` Just "37859"
        it "will return the correct social rank" $ do
            recipe <- getDummyRecipe
            (fmap social_rank recipe) `shouldBe` Just 99.84842829206659
        it "will return the correct publisher url" $ do
            recipe <- getDummyRecipe
            (fmap publisher_url recipe) `shouldBe` Just "http://realsimple.com"
        it "will return the correct title" $ do
            recipe <- getDummyRecipe
            (fmap title recipe) `shouldBe` Just "Chicken and Gruyre Turnovers"
        it "will return the correct publisher" $ do
            recipe <- getDummyRecipe
            (fmap publisher recipe) `shouldBe` Just "Real Simple"
        it "will return the correct f2f url" $ do
            recipe <- getDummyRecipe
            (fmap f2f_url recipe) `shouldBe` Just "http://food2fork.com/view/37859"
        it "will return the correct source url" $ do
            recipe <- getDummyRecipe
            (fmap source_url recipe) `shouldBe` Just "http://www.realsimple.com/food-recipes/browse-all-recipes/chicken-and-gruyere-turnovers-00000000052482/index.html"

main :: IO ()
main = hspec spec
