{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

module Tests where

import Test.QuickCheck (quickCheck)
import Food2ForkAPI as F2F
import Network.HTTP.QueryString
import Data.Map (lookup)

extractQueryParam :: Text -> Text -> Maybe Text
extractQueryParam qs paramName = lookup paramName (parseQuery qs)

queryString :: Text -> Text
queryString url = (split (=='?') url) !! 1

apiKeyFromUrl :: Text -> Maybe Text
apiKeyFromUrl url
    | isURI url = extractQueryParam (queryString url) "key"
    | otherwise = Nothing

prop_searchUrlIsValid :: Text -> Text -> SortOrder -> Int -> Bool
prop_searchUrlIsValid apiKey searchQuery sortOrder pageNumber = 
    let url = F2F.mkSearchUrl apiKey searchQuery sortOrder pageNumber 
     in (apiKey == apiKeyFromUrl url 
      && searchQuery == searchQueryFromUrl url
      && (sortOrderToString sortOrder) == sortOrderFromUrl url
      && pageNumber == pageNumberFromUrl url)

prop_getUrlIsValid :: Text -> Text -> Bool
prop_getUrlIsValid = undefined

main :: IO ()
main = do
    quickCheck prop_searchUrlIsValid
    quickCheck prop_getUrlIsValid
