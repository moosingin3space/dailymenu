{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

module Food2ForkAPI ( Recipe(..)
                    , SearchResult(..)
                    , SortOrder(..)
                    , mkSearchUrl
                    , mkGetUrl
                    , search
                    , getRecipe
                    ) where 

import Control.Monad
import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B
import Utils
import GHC.Generics
import Network.HTTP.QueryString

-- A datatype representing a recipe (from the GetRecipe endpoint)
data Recipe = Recipe { image_url :: !T.Text
                     , source_url :: !T.Text
                     , f2f_url :: !T.Text
                     , recipe_id :: !T.Text
                     , title :: !T.Text
                     , publisher :: !T.Text
                     , publisher_url :: !T.Text
                     , social_rank :: Int
                     , ingredients :: Maybe [T.Text]
                     } deriving (Show, Generic)

instance FromJSON Recipe
instance ToJSON Recipe

-- A datatype representing a search result
data SearchResult = SearchResult { count :: Int
                                 , recipes :: [Recipe]
                                 } deriving (Show, Generic)

instance FromJSON SearchResult
instance ToJSON SearchResult

-- A datatype representing a sort order
data SortOrder = Rating | Trendingness

-- A class for converting to a recipe ID
class ToRecipeId a where
    toRecipeId :: a -> B.ByteString

-- An instance of ToRecipeId for a ShortRecipe
instance ToRecipeId Recipe where
    toRecipeId shortRecipe = encodeUtf8 $ recipe_id shortRecipe

-- An instance of ToRecipeId for a B.ByteString
instance ToRecipeId B.ByteString where
    toRecipeId txt = txt

-- An instance of ToRecipeId for a T.Text
instance ToRecipeId T.Text where
    toRecipeId txt = encodeUtf8 txt

-- Endpoint URLs
searchEndpoint :: B.ByteString
searchEndpoint = "http://food2fork.com/api/search"

getRecipeEndpoint :: B.ByteString
getRecipeEndpoint = "http://food2fork.com/api/get"


instance Show SortOrder where
    show Rating = "r"
    show Trendingness = "t"

mkSearchUrl :: B.ByteString -> B.ByteString -> SortOrder -> Int -> B.ByteString
mkSearchUrl apiKey searchQuery sortOrder pageNumber = 
    searchEndpoint <> "?" <> (toString $ queryString vars)
      where vars = [("key", apiKey),
                    ("q", searchQuery),
                    ("sort", (B.pack $ show sortOrder)),
                    ("page", (intToByteString pageNumber))]

mkGetUrl :: (ToRecipeId a) => B.ByteString -> a -> B.ByteString
mkGetUrl apiKey rId = getRecipeEndpoint <> "?" <> (toString $ queryString vars)
    where vars = [("key", apiKey),
                  ("rId", (toRecipeId rId))]

-- Searches through Food2Fork
-- TODO
search :: B.ByteString -> B.ByteString -> SortOrder -> Int -> IO SearchResult
search apiKey searchQuery sortOrder pageNumber = undefined

-- Gets a particular recipe
-- TODO
getRecipe :: (ToRecipeId a) => B.ByteString -> a -> IO Recipe
getRecipe apiKey rId = undefined
