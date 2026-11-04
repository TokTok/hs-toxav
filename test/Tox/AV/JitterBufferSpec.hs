module Tox.AV.JitterBufferSpec (spec) where

import           Data.List           (foldl')
import           Data.Word           (Word16)
import           Test.Hspec

import           Tox.AV.JitterBuffer

-- | What plays as the frames arrive.
played :: [Word16] -> [(Word16, Maybe Word16)]
played = snd . foldl' step (empty, [])
  where
    step (buffer, out) sequ =
      let (ready, buffer') = drain (insert sequ sequ buffer) in (buffer', out ++ ready)

arrived :: [Word16] -> [(Word16, Maybe Word16)]
arrived = map (\n -> (n, Just n))

spec :: Spec
spec = do
  it "plays frames in order" $
    played [5, 7, 6, 8] `shouldBe` arrived [5, 6, 7, 8]

  it "gives up on a frame once enough later ones wait" $ do
    played [5, 7, 8] `shouldBe` arrived [5]
    played [5, 7, 8, 9] `shouldBe` arrived [5] ++ [(6, Nothing)] ++ arrived [7, 8, 9]

  it "drops late and repeated frames" $
    played [5, 6, 6, 4, 7] `shouldBe` arrived [5, 6, 7]

  it "follows the sequence numbers across the wrap" $
    played [0xFFFE, 0xFFFF, 0, 1] `shouldBe` arrived [0xFFFE, 0xFFFF, 0, 1]

  it "starts over after a jump" $
    played [5, 6, 100, 101] `shouldBe` arrived [5, 6, 100, 101]
