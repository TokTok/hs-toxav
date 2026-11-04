module Tox.AV.RTPSpec (spec) where

import qualified Data.ByteString as BS
import           Data.Maybe      (mapMaybe)
import           Test.Hspec

import           Tox.AV.RTP

frame :: Int -> Frame
frame n = Frame 0x1234 (MediaTime 0xAABBCCDD) 7 keyFrame (BS.pack (take n (cycle [1 .. 251])))

spec :: Spec
spec = do
  it "writes the header toxcore reads" $
    packets Audio (frame 3) `shouldBe`
      [ BS.pack $ [192, 0x80, 0x40, 0x12, 0x34, 0xAA, 0xBB, 0xCC, 0xDD, 0, 0, 0, 7]
          ++ [0, 0, 0, 0, 0, 0, 0, 2] -- flags
          ++ [0, 0, 0, 0] ++ [0, 0, 0, 3] ++ [0, 0, 0, 0] -- offset, length, received
          ++ replicate 44 0
          ++ [0, 0] ++ [0, 3] -- offset and length in 16 bits
          ++ [1, 2, 3]
      ]

  it "cuts a frame into pieces that fit a packet" $ do
    let sent = packets Video (frame 3000)
    map BS.length sent `shouldBe` [1373, 1373, 81 + 3000 - 2 * 1292]
    map pieceOffset (mapMaybe parse sent) `shouldBe` [0, 1292, 2584]
    map BS.length (packets Video (frame maxPieceSize)) `shouldBe` [1373]

  it "reads back what it writes" $ do
    let sent = packets Video (frame 3000)
        pieces = mapMaybe parse sent
    BS.concat (map pieceData pieces) `shouldBe` frameData (frame 3000)
    map pieceTotal pieces `shouldBe` [3000, 3000, 3000]
    map (hasFlags (largeFrame <> keyFrame) . pieceFlags) pieces `shouldBe` [True, True, True]
    map pieceSequence pieces `shouldBe` [0x1234, 0x1234, 0x1234]

  it "drops pieces that don't fit their frame" $ do
    let packet = BS.concat (packets Audio (frame 3))
        corrupt i b = BS.take i packet <> BS.singleton b <> BS.drop (i + 1) packet
    parse packet `shouldSatisfy` (/= Nothing)
    parse (corrupt 2 0x41) `shouldBe` Nothing -- payload type of video
    parse (corrupt 80 2) `shouldBe` Nothing -- shorter than the piece
    parse (BS.take 81 packet) `shouldBe` Nothing -- no data
    parse (BS.take 40 packet) `shouldBe` Nothing

  it "orders sequence numbers across the wrap" $ do
    ahead 0xFFFF 2 `shouldBe` 3
    ahead 2 0xFFFF `shouldBe` (-3)

  it "measures time across the clock's wrap" $ do
    since (MediaTime 5) (MediaTime maxBound) `shouldBe` 6
    since (MediaTime maxBound) (MediaTime 5) `shouldBe` (-6)
