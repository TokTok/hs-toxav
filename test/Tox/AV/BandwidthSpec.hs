module Tox.AV.BandwidthSpec (spec) where

import qualified Data.ByteString  as BS
import           Test.Hspec

import           Tox.AV.Bandwidth

spec :: Spec
spec = do
  it "writes what toxcore reads" $
    encode (Report 0x01020304 0x0A0B0C0D) `shouldBe` BS.pack [196, 1, 2, 3, 4, 10, 11, 12, 13]

  it "reads only reports" $ do
    decode (encode (Report 1 2)) `shouldBe` Just (Report 1 2)
    decode (BS.pack [197, 1, 2, 3, 4, 10, 11, 12, 13]) `shouldBe` Nothing
    decode (BS.pack [196, 1, 2, 3, 4, 10, 11, 12]) `shouldBe` Nothing

  it "reports once an interval with media has passed" $ do
    let m = received 100 (lost 25 (meter 0))
    fst <$> due (interval - 1) m `shouldBe` Nothing
    fst <$> due interval m `shouldBe` Just (Report 25 100)
    fst <$> due interval (meter 0) `shouldBe` Nothing
    loss (Report 25 75) `shouldBe` 0.25
