module Tox.AV.MSISpec (spec) where

import qualified Data.ByteString as BS
import           Test.Hspec
import           Test.QuickCheck (Gen, arbitrary, arbitraryBoundedEnum, forAll,
                                  liftArbitrary)

import           Tox.AV.MSI

genMessage :: Gen Message
genMessage = Message
  <$> arbitraryBoundedEnum
  <*> liftArbitrary arbitraryBoundedEnum
  <*> liftArbitrary (Capabilities <$> arbitrary)

caps :: Capabilities
caps = sendAudio <> receiveAudio <> receiveVideo

spec :: Spec
spec = do
  it "writes what toxcore writes" $ do
    encode (Message Init Nothing (Just caps)) `shouldBe` BS.pack [1, 1, 0, 3, 1, 0x34, 0]
    encode (Message Push Nothing (Just caps)) `shouldBe` BS.pack [1, 1, 1, 3, 1, 0x34, 0]
    encode (Message Pop Nothing Nothing) `shouldBe` BS.pack [1, 1, 2, 0]
    encode (Message Pop (Just Handle) Nothing) `shouldBe` BS.pack [1, 1, 2, 2, 1, 6, 0]

  it "reads what it writes" $ forAll genMessage $ \message ->
    decode (encode message) `shouldBe` Just message

  it "keeps the last of a repeated header" $
    decode (BS.pack [1, 1, 0, 1, 1, 2, 0]) `shouldBe` Just (Message Pop Nothing Nothing)

  it "ignores what follows an early end" $
    decode (BS.pack [1, 1, 2, 0, 9, 9, 0]) `shouldBe` Just (Message Pop Nothing Nothing)

  it "rejects what toxcore rejects" $
    mapM_ ((`shouldBe` Nothing) . decode . BS.pack)
      [ []
      , [1, 1, 2]          -- no end byte
      , [3, 1, 0x34, 0]    -- no request
      , [1, 2, 2, 0]       -- size other than 1
      , [4, 1, 0, 1, 1, 2, 0] -- unknown header
      , [1, 1, 3, 0]       -- unknown request
      , [1, 1, 2, 2, 1, 8, 0] -- unknown error
      , [1, 1, 2, 3, 1, 0] -- cut short
      ]
