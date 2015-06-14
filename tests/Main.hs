{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Test.QuickCheck
import Food2ForkAPI as F2F
import Utils as U
import Network.HTTP.Types.URI

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary

instance Arbitrary F2F.SortOrder where
    arbitrary = elements[F2F.Rating, F2F.Trendingness]

extractQueryParam :: B.ByteString -> B.ByteString -> Maybe B.ByteString
extractQueryParam qs paramName = lookup paramName (parseSimpleQuery qs)

extractQS :: B.ByteString -> B.ByteString
extractQS url = (B.split '?' url) !! 1

apiKeyFromUrl :: B.ByteString -> Maybe B.ByteString
apiKeyFromUrl url = extractQueryParam (extractQS url) "key"

searchQueryFromUrl :: B.ByteString -> Maybe B.ByteString
searchQueryFromUrl url = extractQueryParam (extractQS url) "q"

sortOrderFromUrl :: B.ByteString -> Maybe B.ByteString
sortOrderFromUrl url = extractQueryParam (extractQS url) "sort"

pageNumberFromUrl :: B.ByteString -> Maybe B.ByteString
pageNumberFromUrl url = extractQueryParam (extractQS url) "page"

recipeIdFromUrl :: B.ByteString -> Maybe B.ByteString
recipeIdFromUrl url = extractQueryParam (extractQS url) "rId"

prop_searchUrlIsValid :: B.ByteString -> B.ByteString -> F2F.SortOrder -> Int -> Bool
prop_searchUrlIsValid apiKey searchQuery sortOrder pageNumber = 
    let url = F2F.mkSearchUrl apiKey searchQuery sortOrder pageNumber 
     in (Just apiKey == apiKeyFromUrl url 
      && Just searchQuery == searchQueryFromUrl url
      && Just (B.pack $ show sortOrder) == sortOrderFromUrl url
      && Just (U.intToByteString pageNumber) == pageNumberFromUrl url)

prop_getUrlIsValid :: B.ByteString -> B.ByteString -> Bool
prop_getUrlIsValid apiKey rId = 
    let url = F2F.mkGetUrl apiKey rId
     in (Just apiKey == apiKeyFromUrl url
      && Just rId == recipeIdFromUrl url)

main :: IO ()
main = do
    quickCheck prop_searchUrlIsValid
    quickCheck prop_getUrlIsValid
