{-# LANGUAGE StrictData #-}
-- | Audio frames back in sending order, with a gap given up on once enough
-- later frames are waiting.
module Tox.AV.JitterBuffer
  ( JitterBuffer
  , empty
  , insert
  , drain
  , depth
  , window
  ) where

import qualified Data.Map.Strict as Map
import           Data.Word       (Word16)

import           Tox.AV.RTP      (ahead)

data JitterBuffer a = JitterBuffer
  { bufferNext   :: Maybe Word16
  , bufferFrames :: Map.Map Word16 a
  }

empty :: JitterBuffer a
empty = JitterBuffer Nothing Map.empty

-- | How many frames past a missing one wait before it counts as lost.
depth :: Int
depth = 3

-- | A frame further ahead than this starts the buffer over from it.
window :: Int
window = 16

-- | Late and repeated frames are dropped.
insert :: Word16 -> a -> JitterBuffer a -> JitterBuffer a
insert sequ frame buffer = case bufferNext buffer of
  Nothing -> JitterBuffer (Just sequ) (Map.singleton sequ frame)
  Just next
    | distance < 0 -> buffer
    | distance >= window -> JitterBuffer (Just sequ) (Map.singleton sequ frame)
    | otherwise -> buffer { bufferFrames = Map.insertWith (\_ old -> old) sequ frame (bufferFrames buffer) }
    where
      distance = ahead next sequ

-- | Frames ready to play, in order with their numbers, 'Nothing' for one that
-- was lost.
drain :: JitterBuffer a -> ([(Word16, Maybe a)], JitterBuffer a)
drain buffer = case bufferNext buffer of
  Nothing -> ([], buffer)
  Just next -> case Map.lookup next frames of
    Just frame -> played (Just frame)
    Nothing
      | waiting >= depth -> played Nothing
      | otherwise -> ([], buffer)
    where
      frames = bufferFrames buffer
      waiting = maximum (0 : map (ahead next) (Map.keys frames))
      played frame =
        let (rest, buffer') = drain (JitterBuffer (Just (next + 1)) (Map.delete next frames))
        in ((next, frame) : rest, buffer')
