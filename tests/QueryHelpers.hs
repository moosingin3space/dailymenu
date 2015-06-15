{-# LANGUAGE OverloadedStrings #-}

module QueryHelpers where

import qualified Data.ByteString.Char8 as B
import Network.HTTP.Types.URI

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


