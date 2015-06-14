{# LANGUAGE OverloadedStrings, NoImplicitPrelude, DeriveGeneric,
            BangPatterns #}

module Food2ForkAPI ( Recipe(..)
                    , ShortRecipe(..)
                    , SearchResult(..)
                    , search
                    , getRecipe
                    ) where 

import ClassyPrelude
import Control.Applicative
import Data.Aeson
import Data.Text
import GHC.Generic

-- A datatype representing a recipe (from the GetRecipe endpoint)
data Recipe = Recipe { image_url :: !Text
                     , source_url :: !Text
                     , f2f_url :: !Text
                     , title :: !Text
                     , publisher :: !Text
                     , publisher_url :: !Text
                     , social_rank :: Int
                     , ingredients :: [Text]
                     } deriving (Show, Generic)

instance FromJSON Recipe
instance ToJSON Recipe

-- A datatype representing a recipe (from the SearchRecipe endpoint)
data ShortRecipe = ShortRecipe { publisher :: !Text
                               , f2f_url :: !Text
                               , title :: !Text
                               , source_url :: !Text
                               , recipe_id :: Int
                               , social_rank :: Int
                               , image_url :: !Text
                               , publisher_url :: !Text
                               } deriving (Show, Generic)

instance FromJSON ShortRecipe
instance ToJSON ShortRecipe

-- A datatype representing a search result
data SearchResult = SearchResult { count :: Int
                                 , recipes :: [ShortRecipe]
                                 } deriving (Show, Generic)

-- A datatype representing a sort order
data SortOrder = Rating | Trendingness

-- A class for converting to a recipe ID
class ToRecipeId where
    toRecipeId :: Text

-- An instance of ToRecipeId for a ShortRecipe
instance ToRecipeId ShortRecipe where
    toRecipeId shortRecipe = recipe_id shortRecipe

-- An instance of ToRecipeId for a Text
instance ToRecipeId Text where
    toRecipeId txt = txt

-- Searches through Food2Fork
-- TODO
search :: Text -> Text -> SortOrder -> Int -> SearchResult
search apiKey searchQuery sortOrder pageNumber = undefined

-- Gets a particular recipe
-- TODO
getRecipe :: (ToRecipeId a) => Text -> a -> Recipe
getRecipe apiKey rId = undefined
