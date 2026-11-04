{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
-- | Call signalling messages, as toxcore's msi.c sends them.
module Tox.AV.MSI
  ( Request (..)
  , Error (..)
  , Capabilities (..)
  , sendAudio
  , sendVideo
  , receiveAudio
  , receiveVideo
  , Message (..)
  , encode
  , decode
  ) where

import           Data.Bits       (Bits, (.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word       (Word8)

data Request
  = Init -- ^ Invite.
  | Push -- ^ Answer, or change what we send and accept.
  | Pop  -- ^ Hang up, reject or report an error.
  deriving (Eq, Show, Enum, Bounded)

data Error
  = None
  | InvalidMessage
  | InvalidParam
  | InvalidState
  | StrayMessage
  | System
  | Handle
  | Undisclosed
  deriving (Eq, Show, Enum, Bounded)

-- | What a side sends and accepts. Bits without a name are kept as they came.
newtype Capabilities = Capabilities Word8
  deriving (Eq, Show, Bits)

sendAudio, sendVideo, receiveAudio, receiveVideo :: Capabilities
sendAudio = Capabilities 0x04
sendVideo = Capabilities 0x08
receiveAudio = Capabilities 0x10
receiveVideo = Capabilities 0x20

instance Semigroup Capabilities where
  (<>) = (.|.)

instance Monoid Capabilities where
  mempty = Capabilities 0

data Message = Message
  { messageRequest      :: Request
  , messageError        :: Maybe Error
  , messageCapabilities :: Maybe Capabilities
  } deriving (Eq, Show)

headerRequest, headerError, headerCapabilities :: Word8
headerRequest = 1
headerError = 2
headerCapabilities = 3

-- | Each header is an id, a size that is always 1, and the value. A 0 ends
-- the message.
encode :: Message -> ByteString
encode (Message request err caps) = BS.pack $
  header headerRequest (Just (fromEnumByte request))
  ++ header headerError (fromEnumByte <$> err)
  ++ header headerCapabilities ((\(Capabilities c) -> c) <$> caps)
  ++ [0]
  where
    header ident = maybe [] (\value -> [ident, 1, value])

-- | Like toxcore: the last byte must be 0, a header seen twice keeps its last
-- value, and bytes after a 0 where a header would start are ignored.
decode :: ByteString -> Maybe Message
decode bytes
  | BS.null bytes || BS.last bytes /= 0 = Nothing
  | otherwise = go Nothing Nothing Nothing (BS.unpack (BS.init bytes))
  where
    go request err caps rest = case rest of
      ident : 1 : value : rest'
        | ident == headerRequest -> do
            r <- toEnumByte value
            go (Just r) err caps rest'
        | ident == headerError -> do
            e <- toEnumByte value
            go request (Just e) caps rest'
        | ident == headerCapabilities -> go request err (Just (Capabilities value)) rest'
      0 : _ -> done
      []    -> done
      _     -> Nothing
      where
        done = (\r -> Message r err caps) <$> request

fromEnumByte :: Enum a => a -> Word8
fromEnumByte = fromIntegral . fromEnum

toEnumByte :: forall a. (Enum a, Bounded a) => Word8 -> Maybe a
toEnumByte value
  | fromIntegral value <= fromEnum (maxBound :: a) = Just (toEnum (fromIntegral value))
  | otherwise = Nothing
