{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | A Tox client for interoperability tests. It adds a friend by Tox ID,
-- calls it with --call or answers its call with --answer, sends audio and
-- video for a few seconds, then hangs up. It prints what happens and what
-- comes back.
module Main (main) where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad             (forM_, forever, void, when)
import qualified Crypto.Saltine.Class      as Sodium
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import qualified Data.ByteString.Char8     as BS8
import           Data.IORef                (atomicModifyIORef', newIORef)
import qualified Data.Vector.Storable      as V
import           Data.Word                 (Word16, Word8)
import           System.Environment        (getArgs)
import           System.IO                 (BufferMode (..), hSetBuffering,
                                            stdout)

import qualified Tox.Application.Tox       as Tox
import qualified Tox.AV                    as AV
import           Tox.AV                    (BitRates (..), Control (..),
                                            Event (..), Picture (..), ToxAV,
                                            Transport (..))
import           Tox.Crypto.Core.Key       (PublicKey)
import           Tox.Network.Core.NodeInfo (NodeInfo)

data Role = Caller | Callee
  deriving (Eq, Show)

rates :: BitRates
rates = BitRates 48 1000

-- | How long we send media before hanging up.
callSeconds :: Int
callSeconds = 3

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  (node, address, role) <- case args of
    ["--bootstrap", node, "--add", address, flag]
      | flag == "--call" -> pure (node, address, Caller)
      | flag == "--answer" -> pure (node, address, Callee)
    _ -> fail "usage: tox-av-peer --bootstrap HOST:PORT:DHT_KEY --add TOX_ID (--call | --answer)"
  tox <- Tox.new Tox.defaultOptions { Tox.optLocalDiscovery = False } >>= either (fail . show) pure
  av <- AV.new Transport
    { sendLossless = Tox.friendQueueLosslessPacket tox
    , sendLossy = Tox.friendQueueLossyPacket tox
    }
  resolve node >>= mapM_ (Tox.bootstrap tox)
  bytes <- either fail pure (Base16.decode (BS8.pack address))
  void $ Tox.friendAdd tox bytes (BS8.pack "Hello from Haskell") >>= either (fail . show) pure
  called <- newIORef False
  void . forkIO . forever $ AV.nextEvent av >>= handle av role
  forever $ Tox.nextEvent tox >>= \case
    Tox.FriendConnectionChanged pk status -> do
      let online = status /= Tox.NotConnected
      AV.friendConnection av pk online
      first <- if online then atomicModifyIORef' called (\c -> (True, not c)) else pure False
      when (first && role == Caller) $ AV.call av pk rates >>= putStrLn . ("CALL " ++) . show
    Tox.FriendLosslessPacket pk packet -> forward av AV.losslessIds pk packet
    Tox.FriendLossyPacket pk packet -> forward av AV.lossyIds pk packet
    _ -> pure ()

forward :: ToxAV PublicKey -> [Word8] -> PublicKey -> BS.ByteString -> IO ()
forward av ids pk packet = forM_ (BS.uncons packet) $ \(ident, _) ->
  when (ident `elem` ids) $ AV.receivePacket av pk packet

handle :: ToxAV PublicKey -> Role -> Event PublicKey -> IO ()
handle av role = \case
  Invited pk caps -> do
    putStrLn $ "INVITED " ++ show (AV.sendsAudio caps) ++ " " ++ show (AV.sendsVideo caps)
    when (role == Callee) $ do
      answered <- AV.answer av pk rates
      putStrLn $ "ANSWER " ++ show answered
      when (answered == Right ()) . void . forkIO $ talk av pk
  PeerChanged pk caps -> do
    putStrLn $ "PEER_CHANGED " ++ show (AV.sendsAudio caps) ++ " " ++ show (AV.sendsVideo caps)
    when (role == Caller) . void . forkIO $ talk av pk
  Ended _ end -> putStrLn $ "ENDED " ++ show end
  AudioReceived _ _ _ (AV.AudioFrame rate channels pcm) ->
    putStrLn $ "AUDIO_RECEIVED " ++ show rate ++ " " ++ show channels ++ " " ++ show (V.length pcm `div` channels)
  VideoReceived _ _ got ->
    putStrLn $ "VIDEO_RECEIVED " ++ show (pictureWidth got) ++ " " ++ show (pictureHeight got)

-- | 20 ms of 48 kHz mono audio, and a video frame every 100 ms, then hang up.
talk :: ToxAV PublicKey -> PublicKey -> IO ()
talk av pk = do
  forM_ [0 .. callSeconds * 50 - 1] $ \i -> do
    captured <- AV.clock
    sent <- AV.sendAudio av pk captured (AV.AudioFrame 48000 1 (tone i))
    when (i == 0) $ putStrLn $ "AUDIO_SENT " ++ show sent
    when (i `mod` 5 == 0) $ do
      shown <- AV.sendVideo av pk captured (picture i)
      when (i == 0) $ putStrLn $ "VIDEO_SENT " ++ show shown
    threadDelay 20000
  AV.control av pk Cancel >>= putStrLn . ("HUNG_UP " ++) . show

tone :: Int -> AV.Pcm
tone i = V.generate 960 $ \n ->
  round (8000 * sin (2 * pi * 440 * fromIntegral (i * 960 + n) / 48000 :: Double))

picture :: Int -> Picture
picture i = Picture 160 120
  (BS.pack [fromIntegral (x + y + i) | y <- [1 .. 120 :: Int], x <- [1 .. 160]])
  (BS.replicate (80 * 60) 100)
  (BS.replicate (80 * 60) 150)

resolve :: String -> IO [NodeInfo]
resolve spec = case splitOn ':' spec of
  [host, port, key] | Right bytes <- Base16.decode (BS8.pack key), Just pk <- Sodium.decode bytes ->
    Tox.resolveNode host (read port :: Word16) pk
  _ -> fail ("invalid node: " ++ spec)
  where
    splitOn c s = case break (== c) s of
      (a, [])       -> [a]
      (a, _ : rest) -> a : splitOn c rest
