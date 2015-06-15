{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import qualified Data.ByteString.Char8 as B
import Food2ForkAPI 
import Test.QuickCheck

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary

instance Arbitrary SortOrder where
    arbitrary = elements[Rating, Trendingness]
