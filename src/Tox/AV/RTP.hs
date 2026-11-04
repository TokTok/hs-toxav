{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
-- | Audio and video frames on the wire: an 80-byte header, then the frame,
-- cut into pieces that each fit in one lossy packet.
module Tox.AV.RTP
  ( Media (..)
  , packetId
  , media
  , Flags (..)
  , largeFrame
  , keyFrame
  , h264
  , h265
  , hasFlags
  , Codec (..)
  , codecFlags
  , codecOf
  , MediaTime (..)
  , since
  , later
  , Frame (..)
  , packets
  , Piece (..)
  , parse
  , maxPieceSize
  , ahead
  ) where

import           Control.Monad           (guard)
import           Data.Binary.Get         (Get, getWord16be, getWord32be,
                                          getWord64be, getWord8, runGetOrFail,
                                          skip)
import           Data.Bits               (Bits, (.&.), (.|.))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import           Data.Int                (Int16, Int32)
import           Data.Word               (Word16, Word32, Word64, Word8)

data Media = Audio | Video
  deriving (Eq, Ord, Show, Enum, Bounded)

packetId :: Media -> Word8
packetId Audio = 192
packetId Video = 193

media :: Word8 -> Maybe Media
media ident = case ident of
  192 -> Just Audio
  193 -> Just Video
  _   -> Nothing

newtype Flags = Flags Word64
  deriving (Eq, Show, Bits)

instance Semigroup Flags where
  (<>) = (.|.)

instance Monoid Flags where
  mempty = Flags 0

-- | The frame's length and offset are in the 32-bit fields. Always set on
-- video.
largeFrame :: Flags
largeFrame = Flags 0x01

keyFrame :: Flags
keyFrame = Flags 0x02

-- | The frame is H.264 or H.265, not VP8.
h264, h265 :: Flags
h264 = Flags 0x04
h265 = Flags 0x40

hasFlags :: Flags -> Flags -> Bool
hasFlags (Flags these) (Flags flags) = flags .&. these == these

data Codec = VP8 | H264 | H265
  deriving (Eq, Ord, Show, Enum, Bounded)

codecFlags :: Codec -> Flags
codecFlags codec = case codec of
  VP8  -> mempty
  H264 -> h264
  H265 -> h265

codecOf :: Flags -> Codec
codecOf flags
  | hasFlags h264 flags = H264
  | hasFlags h265 flags = H265
  | otherwise = VP8

-- | Milliseconds on the sender's clock, which wraps. Only times from one
-- sender compare, as when lining up its audio and video.
newtype MediaTime = MediaTime Word32
  deriving (Eq, Ord, Show)

-- | How many milliseconds the first time comes after the second.
since :: MediaTime -> MediaTime -> Int
since (MediaTime a) (MediaTime b) = fromIntegral (fromIntegral (a - b) :: Int32)

-- | Some milliseconds later, or earlier if negative.
later :: Int -> MediaTime -> MediaTime
later ms (MediaTime t) = MediaTime (t + fromIntegral ms)

data Frame = Frame
  { frameSequence  :: Word16
  , frameTimestamp :: MediaTime
  , frameSsrc      :: Word32
  , frameFlags     :: Flags
  , frameData      :: ByteString
  } deriving (Eq, Show)

headerSize, maxPacketSize, maxPieceSize :: Int
headerSize = 80
-- | Largest lossy packet toxcore sends, its id included.
maxPacketSize = 1373
maxPieceSize = maxPacketSize - 1 - headerSize

-- | Ids included, in order.
packets :: Media -> Frame -> [ByteString]
packets m (Frame sequ (MediaTime timestamp) ssrc flags bytes) =
  [ LBS.toStrict . B.toLazyByteString $ header offset <> B.byteString piece
  | (offset, piece) <- pieces ]
  where
    total = BS.length bytes
    pieces
      | BS.null bytes = [(0, BS.empty)]
      | otherwise =
          [ (offset, BS.take maxPieceSize (BS.drop offset bytes))
          | offset <- [0, maxPieceSize .. total - 1] ]
    flags' = if m == Video then flags <> largeFrame else flags
    header offset = mconcat
      [ B.word8 (packetId m)
      , B.word8 0x80 -- Version 2.
      , B.word8 (packetId m .&. 0x7F)
      , B.word16BE sequ
      , B.word32BE timestamp
      , B.word32BE ssrc
      , B.word64BE ((\(Flags f) -> f) flags')
      , B.word32BE (fromIntegral offset)
      , B.word32BE (fromIntegral total)
      , B.word32BE 0
      , B.byteString (BS.replicate 44 0)
      , B.word16BE (fromIntegral offset)
      , B.word16BE (fromIntegral (min total 0xFFFF))
      ]

data Piece = Piece
  { pieceMedia     :: Media
  , pieceSequence  :: Word16
  , pieceTimestamp :: MediaTime
  , pieceFlags     :: Flags
  , pieceOffset    :: Int
  , pieceTotal     :: Int -- ^ Whole frame's length.
  , pieceData      :: ByteString
  } deriving (Eq, Show)

-- | A packet, its id included, that holds a non-empty piece lying within its
-- frame.
parse :: ByteString -> Maybe Piece
parse packet = do
  (ident, rest) <- BS.uncons packet
  m <- media ident
  (piece, header) <- either (const Nothing) (\(r, _, h) -> Just (LBS.toStrict r, h))
    (runGetOrFail (getHeader m) (LBS.fromStrict rest))
  let piece' = header { pieceData = piece }
      end = pieceOffset piece' + BS.length piece
  guard $ not (BS.null piece) && end <= pieceTotal piece'
  pure piece'

getHeader :: Media -> Get Piece
getHeader m = do
  _ <- getWord8
  payloadType <- getWord8
  guard $ payloadType .&. 0x7F == packetId m .&. 0x7F
  sequ <- getWord16be
  timestamp <- getWord32be
  skip 4 -- SSRC
  flags <- Flags <$> getWord64be
  offset <- getWord32be
  total <- getWord32be
  skip 48 -- Received length and padding.
  offsetLower <- getWord16be
  totalLower <- getWord16be
  let (offset', total')
        | hasFlags largeFrame flags = (offset, total)
        | otherwise = (fromIntegral offsetLower, fromIntegral totalLower)
  pure $ Piece m sequ (MediaTime timestamp) flags (fromIntegral offset') (fromIntegral total') BS.empty

-- | How many frames the second sequence number comes after the first,
-- negative for before.
ahead :: Word16 -> Word16 -> Int
ahead from to = fromIntegral (fromIntegral (to - from) :: Int16)
