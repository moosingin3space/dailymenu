{-# LANGUAGE OverloadedStrings #-}
module StorageSpec where

import SpecHelper

import qualified Data.Text as T
import Storage

prop_underscoreize_reverseable :: T.Text -> Bool
prop_underscoreize_reverseable text =
    T.replace "__" " " (underscoreize text) == text

spec :: Spec
spec = do
    describe "The underscoreize function" $ do
        it "should be reverseable" $ property $
          prop_underscoreize_reverseable

main :: IO ()
main = hspec spec
