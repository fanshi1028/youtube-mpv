{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (race)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson as JSON (KeyValue ((.=)), ToJSON (toEncoding, toJSON), encode, pairs)
import Data.Binary as B (Binary (get, put))
import Data.Binary.Get (getLazyByteString, getWord32host, runGetOrFail)
import Data.Binary.Put (execPut, putLazyByteString, putWord32host)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy as BL (ByteString, getContents, length)
import Data.ByteString.Lazy.Char8 as BL (lines)
import Data.Foldable (traverse_)
import Data.Text as T (Text, pack)
import Network.Socket (Family (AF_UNIX), SocketType (Datagram), close, defaultProtocol, socketPair, withFdSocket)
import Network.Socket.ByteString.Lazy as SOC (getContents, sendAll)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (BlockBuffering), hFlush, hSetBinaryMode, hSetBuffering, stdin, stdout)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (NoStream), shell, waitForProcess, withCreateProcess)

newtype NativeMessage a = NativeMessage a deriving (Show, Eq, ToJSON)

instance Binary (NativeMessage BL.ByteString) where
  put (NativeMessage bs) = do
    putWord32host . fromIntegral $ BL.length bs
    putLazyByteString bs
  get =
    NativeMessage <$> do
      getWord32host >>= getLazyByteString . fromIntegral

instance {-# OVERLAPPABLE #-} (ToJSON a) => Binary (NativeMessage a) where
  put (NativeMessage err) = put . NativeMessage $ JSON.encode err
  get = undefined

hEncodeAndSend :: (Binary (NativeMessage a)) => a -> IO ()
hEncodeAndSend a = hPutBuilder stdout . execPut . put $ NativeMessage a

newtype MPVError = MPVError Text deriving (Show, Eq)

instance ToJSON MPVError where
  toJSON = undefined
  toEncoding (MPVError err) = pairs $ "error" .= err

newtype MPVInfo = MPVInfo Text deriving (Show, Eq)

instance ToJSON MPVInfo where
  toJSON = undefined
  toEncoding (MPVInfo info) = pairs $ "info" .= info

main :: IO ()
main = do
  hSetBuffering stdin $ BlockBuffering Nothing
  hSetBinaryMode stdin True
  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBinaryMode stdout True
  bracket (socketPair AF_UNIX Datagram defaultProtocol) (\(soc1, soc2) -> close soc1 *> close soc2) $ \(soc1, soc2) -> do
    let runMpvIPC = withFdSocket soc2 $ \fd -> do
          let process = (shell $ "mpv --input-ipc-client=fd://" <> show fd <> " --idle --keep-open --profile=youtube-mpv") {std_in = NoStream, std_out = NoStream, std_err = NoStream}
          withCreateProcess process $ \cases
            (Just _) _ _ _ -> pure . Left $ MPVError "impossible: std_in handle exists for mpv"
            _ (Just _) _ _ -> pure . Left $ MPVError "impossible: std_out handle exists for mpv"
            _ _ (Just _) _ -> pure . Left $ MPVError "impossible: std_err handle exists for mpv"
            Nothing Nothing Nothing h ->
              let parseAndSendCommand l = case runGetOrFail get l of
                    Right (uncomsumed, b, NativeMessage cmd) -> do
                      hEncodeAndSend . MPVInfo $ "Comsumed " <> T.pack (show b) <> " bytes."
                      hEncodeAndSend cmd
                      sendAll soc1 cmd
                      sendAll soc1 "\n"
                      parseAndSendCommand uncomsumed
                    Left _ -> pure ()
                  commandLoop = BL.getContents >>= parseAndSendCommand
               in (MPVError "commadLoop ended before mpv" <$ commandLoop) `race` waitForProcess h
        mpvOutputPrintloop = SOC.getContents soc1 >>= traverse_ ((*> hFlush stdout) . hEncodeAndSend) . BL.lines
    try ((MPVError "mpvOutputPrintloop ended before mpv" <$ mpvOutputPrintloop) `race` runMpvIPC) >>= \case
      Left (err :: SomeException) -> hEncodeAndSend . MPVError . T.pack $ show err
      Right (Right (Right ExitSuccess)) -> hEncodeAndSend $ MPVInfo "Done, all good"
      Right (Right (Right oops)) -> hEncodeAndSend . MPVError . T.pack $ show oops
      Right (Right (Left oops)) -> hEncodeAndSend oops
      Right (Left err) -> hEncodeAndSend err
