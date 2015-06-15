{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

module Food2ForkAPI ( Recipe(..)
                    , SearchResult(..)
                    , SortOrder(..)
                    , mkSearchUrl
                    , mkGetUrl
                    , search
                    , getRecipe
                    ) where 

import Data.Aeson
import Data.Functor
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Utils
import GHC.Generics
import Network.HTTP.Types

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

-- A type for a simple HTTP GET function
type HttpGet = B.ByteString -> IO (Maybe BL.ByteString)

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
    searchEndpoint <> (renderQuery True vars)
      where vars = [("key", Just apiKey),
                    ("q", Just searchQuery),
                    ("sort", Just $ B.pack $ show sortOrder),
                    ("page", Just $ intToByteString pageNumber)]

mkGetUrl :: (ToRecipeId a) => B.ByteString -> a -> B.ByteString
mkGetUrl apiKey rId = getRecipeEndpoint <> (renderQuery True vars)
    where vars = [("key", Just apiKey),
                  ("rId", Just $ toRecipeId rId)]

-- Searches through Food2Fork
search :: HttpGet -> B.ByteString -> B.ByteString -> SortOrder -> Int -> IO (Maybe SearchResult)
search getter apiKey searchQuery sortOrder pageNumber = do
    result <- getter $ mkSearchUrl apiKey searchQuery sortOrder pageNumber
    case result of
      Just r -> return $ decode r
      Nothing -> return $ Nothing

-- Gets a particular recipe
getRecipe :: (ToRecipeId a) => HttpGet -> B.ByteString -> a -> IO (Maybe Recipe)
getRecipe getter apiKey rId = do
    result <- getter $ mkGetUrl apiKey rId
    case result of
      Just r -> return $ decode r
      Nothing -> return $ Nothing
