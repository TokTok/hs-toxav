{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE StrictData               #-}
-- | Video through libavcodec.
module Tox.AV.Codec.Video
  ( VideoEncoder
  , videoEncoder
  , encodeVideo
  , VideoDecoder
  , videoDecoder
  , decodeVideo
  , encoders
  , decoders
  ) where

import           Control.Monad          (filterM, when, (<=<))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Int               (Int64)
import           Data.Word              (Word64, Word8)
import           Foreign.C.String       (CString, withCString)
import           Foreign.C.Types        (CInt (..))
import           Foreign.ForeignPtr     (ForeignPtr, finalizeForeignPtr,
                                         withForeignPtr)
import           Foreign.Marshal.Alloc  (alloca)
import           Foreign.Ptr            (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import           Foreign.Storable       (peek)

import           Tox.AV.Codec.Types     (Close (..), Picture (..), chromaSize,
                                         complete, keyFrameInterval, managed)
import           Tox.AV.RTP             (Codec (..), MediaTime (..))

data VideoStruct

-- Encoding, decoding and opening a codec take long, so those calls are safe:
-- an unsafe one holds up every other Haskell thread until it returns.
foreign import ccall safe "hstoxav_video_encoder_new"
  c_video_encoder_new :: CString -> CInt -> CInt -> Int64 -> CInt -> IO (Ptr VideoStruct)
foreign import ccall safe "hstoxav_video_decoder_new"
  c_video_decoder_new :: CString -> Int64 -> IO (Ptr VideoStruct)
foreign import ccall unsafe "&hstoxav_video_free"
  c_video_free :: FunPtr (Ptr VideoStruct -> IO ())
foreign import ccall safe "hstoxav_video_send_frame"
  c_video_send_frame :: Ptr VideoStruct -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int64 -> CInt -> IO CInt
foreign import ccall safe "hstoxav_video_receive_packet"
  c_video_receive_packet :: Ptr VideoStruct -> Ptr (Ptr Word8) -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall safe "hstoxav_video_send_packet"
  c_video_send_packet :: Ptr VideoStruct -> Ptr Word8 -> CInt -> Int64 -> IO CInt
foreign import ccall safe "hstoxav_video_receive_frame"
  c_video_receive_frame
    :: Ptr VideoStruct -> Ptr CInt -> Ptr CInt -> Ptr (Ptr Word8) -> Ptr (Ptr Word8) -> Ptr (Ptr Word8)
    -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr Int64 -> IO CInt
foreign import ccall unsafe "avcodec_find_encoder_by_name"
  c_avcodec_find_encoder_by_name :: CString -> IO (Ptr ())
foreign import ccall unsafe "avcodec_find_decoder_by_name"
  c_avcodec_find_decoder_by_name :: CString -> IO (Ptr ())

instance Close VideoEncoder where
  close (VideoEncoder enc) = finalizeForeignPtr enc

instance Close VideoDecoder where
  close (VideoDecoder dec _) = finalizeForeignPtr dec

encoderNames, decoderNames :: Codec -> [String]
encoderNames codec = case codec of
  VP8  -> ["libvpx"]
  H264 -> ["h264_mediacodec", "libopenh264", "libx264"]
  H265 -> ["hevc_mediacodec", "libx265"]
decoderNames codec = case codec of
  VP8  -> ["vp8"]
  H264 -> ["h264"]
  H265 -> ["hevc"]

available :: (CString -> IO (Ptr ())) -> (Codec -> [String]) -> IO [Codec]
available find names = filterM (fmap (not . null) . filterM found . names) [minBound .. maxBound]
  where
    found name = (/= nullPtr) <$> withCString name find

encoders, decoders :: IO [Codec]
encoders = available c_avcodec_find_encoder_by_name encoderNames
decoders = available c_avcodec_find_decoder_by_name decoderNames

-- | Stops at the first that works.
firstJust :: [IO (Maybe a)] -> IO (Maybe a)
firstJust []       = pure Nothing
firstJust (a : as) = a >>= maybe (firstJust as) (pure . Just)

newtype VideoEncoder = VideoEncoder (ForeignPtr VideoStruct)

-- | For pictures of a size, in bits per second.
videoEncoder :: Codec -> Int -> Int -> Int -> IO (Maybe VideoEncoder)
videoEncoder codec width height bitRate = fmap VideoEncoder <$> firstJust
  [ withCString name $ \cname -> managed c_video_free =<<
      c_video_encoder_new cname (fromIntegral width) (fromIntegral height) (fromIntegral bitRate)
        (fromIntegral (keyFrameInterval codec))
  | name <- encoderNames codec ]

-- | Each frame comes with whether it's a key frame. The time is in
-- milliseconds.
encodeVideo :: VideoEncoder -> Word64 -> Bool -> Picture -> IO (Maybe [(Bool, ByteString)])
encodeVideo (VideoEncoder enc) time key picture@(Picture _ _ y u v)
  | not (complete picture) = pure Nothing
  | otherwise = withForeignPtr enc $ \ptr ->
      BS.unsafeUseAsCString y $ \py -> BS.unsafeUseAsCString u $ \pu -> BS.unsafeUseAsCString v $ \pv -> do
        sent <- c_video_send_frame ptr (castPtr py) (castPtr pu) (castPtr pv) (fromIntegral time) (if key then 1 else 0)
        if sent < 0 then pure Nothing else receive ptr []
  where
    receive ptr frames = alloca $ \pdata -> alloca $ \psize -> alloca $ \pkey -> do
      ret <- c_video_receive_packet ptr pdata psize pkey
      case ret of
        0 -> do
          bytes <- peek pdata
          size <- peek psize
          isKey <- peek pkey
          frame <- BS.packCStringLen (castPtr bytes, fromIntegral size)
          receive ptr ((isKey /= 0, frame) : frames)
        1 -> pure (Just (reverse frames))
        _ -> pure Nothing

data VideoDecoder = VideoDecoder (ForeignPtr VideoStruct) ((MediaTime, Picture) -> IO ())

-- | Pixel limit, roughly (decoders count the padding they add to the
-- width), and where each picture goes once decoded, with the time of the
-- frame it's from.
videoDecoder :: Codec -> Int -> ((MediaTime, Picture) -> IO ()) -> IO (Maybe VideoDecoder)
videoDecoder codec maxPixels deliver = fmap (`VideoDecoder` deliver) <$> firstJust
  [ withCString name (managed c_video_free <=< (`c_video_decoder_new` fromIntegral maxPixels))
  | name <- decoderNames codec ]

-- | The frame's pictures go where the decoder sends them, before this
-- returns. False when it couldn't decode the frame.
decodeVideo :: VideoDecoder -> MediaTime -> ByteString -> IO Bool
decodeVideo (VideoDecoder dec deliver) (MediaTime time) frame = withForeignPtr dec $ \ptr ->
  BS.unsafeUseAsCStringLen frame $ \(bytes, len) -> do
    sent <- c_video_send_packet ptr (castPtr bytes) (fromIntegral len) (fromIntegral time)
    if sent < 0 then pure False else receive ptr
  where
    receive ptr =
      alloca $ \pw -> alloca $ \ph -> alloca $ \py -> alloca $ \pu -> alloca $ \pv ->
      alloca $ \sy -> alloca $ \su -> alloca $ \sv -> alloca $ \pt -> do
        ret <- c_video_receive_frame ptr pw ph py pu pv sy su sv pt
        case ret of
          0 -> do
            w <- fromIntegral <$> peek pw
            h <- fromIntegral <$> peek ph
            y <- plane py sy w h
            u <- plane pu su (chromaSize w) (chromaSize h)
            v <- plane pv sv (chromaSize w) (chromaSize h)
            t <- MediaTime . fromIntegral <$> peek pt
            deliver (t, Picture w h y u v)
            receive ptr
          1 -> pure True
          2 -> receive ptr
          _ -> pure False
    plane pptr pstride w h = do
      base <- peek pptr
      stride <- fromIntegral <$> peek pstride
      BS.concat <$> sequence
        [ BS.packCStringLen (castPtr base `plusPtr` (row * stride), w) | row <- [0 .. h - 1] ]
