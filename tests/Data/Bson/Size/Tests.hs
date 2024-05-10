{-# LANGUAGE FlexibleContexts #-}
module Data.Bson.Size.Tests 
    ( tests
    ) where

import Data.Binary.Put (runPut)
import Data.Bson
import Data.Bson.Binary (putDocument)
import Data.Bson.Size (sizeOfDocument)
import qualified Data.ByteString.Lazy as BL
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (TestOptions' (..), plusTestOptions)
import Test.QuickCheck (Arbitrary(..))

testDoc :: Document -> Bool
testDoc doc =
    sizeOfDocument doc == (fromIntegral $ BL.length $ runPut $ putDocument doc)

withTestOpts :: Test -> Test
withTestOpts = plusTestOptions $ TestOptions Nothing Nothing Nothing (Just 13) (Just 5) Nothing

tests :: Arbitrary Field => Test
tests = testGroup "Data.Bson.Size.Tests"
  [ withTestOpts $ testProperty "sizeOfDocument" testDoc
  ]
