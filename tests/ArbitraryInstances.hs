{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Food2ForkAPI 
import Test.QuickCheck
import Control.Applicative

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary

instance Arbitrary T.Text where
    arbitrary = fmap T.pack arbitrary

instance Arbitrary SortOrder where
    arbitrary = elements[Rating, Trendingness]

instance Arbitrary Recipe where
    arbitrary = Recipe <$> arbitrary 
                       <*> arbitrary 
                       <*> arbitrary 
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
