{-# LANGUAGE StrictData #-}
-- | Frames put back together from their pieces.
module Tox.AV.Assembler
  ( Assembler
  , empty
  , Outcome (..)
  , add
  , maxPending
  , maxFrameSize
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List       (minimumBy)
import qualified Data.Map.Strict as Map
import           Data.Ord        (comparing)
import           Data.Word       (Word16)

import           Tox.AV.RTP      (Flags, MediaTime, Piece (..), maxPieceSize)

-- | Pieces of one frame, by offset.
data Partial = Partial
  { partialArrival  :: Int
  , partialTotal    :: Int
  , partialFlags    :: Flags
  , partialPieces   :: Map.Map Int ByteString
  , partialReceived :: Int
  }

-- | Frames are told apart by sequence number and timestamp.
type Key = (Word16, MediaTime)

data Assembler = Assembler
  { assemblerArrivals :: Int
  , assemblerPending  :: Map.Map Key Partial
  }

empty :: Assembler
empty = Assembler 0 Map.empty

data Outcome
  = Complete Word16 MediaTime Flags ByteString
  | Incomplete Int -- ^ The bytes missing from a frame that was given up on.
  deriving (Eq, Show)

-- | How many frames can be waiting for pieces. A new one pushes out the
-- frame that started first.
maxPending :: Int
maxPending = 3

maxFrameSize :: Int
maxFrameSize = 10 * 1024 * 1024

-- | Toxcore fills a packet with each piece of a frame but its last. Shorter
-- ones are taken down to half that, and no shorter, so that a frame's pieces
-- can't cost much more than its bytes.
minPieceSize :: Int
minPieceSize = maxPieceSize `div` 2

add :: Piece -> Assembler -> (Assembler, [Outcome])
add piece assembler
  | BS.length (pieceData piece) < minPieceSize
  , pieceOffset piece + BS.length (pieceData piece) /= pieceTotal piece = (assembler, [])
  | otherwise = case Map.lookup key pending of
      Just partial
        | partialTotal partial /= pieceTotal piece -> (assembler, [])
        | Map.member (pieceOffset piece) (partialPieces partial) -> (assembler, [])
        | otherwise -> fill partial assembler { assemblerPending = Map.delete key pending }
      Nothing
        | pieceTotal piece > maxFrameSize -> (assembler, [])
        | otherwise ->
            let (room, evicted) = makeRoom assembler
            in (evicted ++) <$> fill (newPartial (assemblerArrivals room)) room
                 { assemblerArrivals = assemblerArrivals room + 1 }
  where
    key = (pieceSequence piece, pieceTimestamp piece)
    pending = assemblerPending assembler
    newPartial arrival = Partial arrival (pieceTotal piece) (pieceFlags piece) Map.empty 0
    fill partial a
      | partialReceived partial' < partialTotal partial' =
          (a { assemblerPending = Map.insert key partial' (assemblerPending a) }, [])
      | otherwise = (a, [assemble partial'])
      where
        partial' = partial
          -- A copy, as the piece shares the whole packet's bytes.
          { partialPieces = Map.insert (pieceOffset piece) (BS.copy (pieceData piece)) (partialPieces partial)
          , partialReceived = partialReceived partial + BS.length (pieceData piece)
          }
    assemble partial
      | contiguous = Complete (pieceSequence piece) (pieceTimestamp piece) (partialFlags partial) (BS.concat (Map.elems pieces))
      | otherwise = Incomplete (partialTotal partial)
      where
        pieces = partialPieces partial
        ends = scanl (+) 0 (map BS.length (Map.elems pieces))
        contiguous = and (zipWith (==) (Map.keys pieces) ends) && last ends == partialTotal partial

makeRoom :: Assembler -> (Assembler, [Outcome])
makeRoom assembler
  | Map.size pending < maxPending = (assembler, [])
  | otherwise =
      let (key, partial) = minimumBy (comparing (partialArrival . snd)) (Map.toList pending)
      in ( assembler { assemblerPending = Map.delete key pending }
         , [Incomplete (partialTotal partial - partialReceived partial)] )
  where
    pending = assemblerPending assembler
