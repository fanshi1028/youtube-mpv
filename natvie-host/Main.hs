{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toEncoding, toJSON), eitherDecode, encode, pairs, withObject, (.:))
import Data.Binary (Binary (get, put))
import Data.Binary qualified as B (encode)
import Data.Binary.Get (getLazyByteString, getWord32host, runGetOrFail)
import Data.Binary.Put (execPut, putLazyByteString, putWord32host)
import Data.ByteString (toStrict)
import Data.ByteString qualified as BS (putStr)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy qualified as BL (getContents, length)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (BlockBuffering), IOMode (WriteMode), hGetContents, hPutStrLn, hSetBinaryMode, hSetBuffering, hWaitForInput, stdin, stdout, withFile)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (CreatePipe, NoStream), shell, waitForProcess, withCreateProcess)

newtype NativeMessage a = NativeMessage a deriving (Show, Eq, ToJSON)

newtype VideoId = VideoId
  { unVideoId :: Text
  }
  deriving (Show, Eq)

instance FromJSON VideoId where
  parseJSON = withObject "Video Id" $ \o -> VideoId <$> o .: "v"

instance ToJSON VideoId where
  toJSON = undefined
  toEncoding (VideoId vid) = pairs $ "v" .= vid

data MPVReply = MPVError Text | MPVOK deriving (Show, Eq)

instance FromJSON MPVReply where
  parseJSON = undefined

instance ToJSON MPVReply where
  toJSON = undefined
  toEncoding = \case
    MPVError err -> pairs $ "error" .= err
    MPVOK -> pairs $ "mpv" .= ("ok" :: Text)

instance (ToJSON a, FromJSON a) => Binary (NativeMessage a) where
  put (NativeMessage a) = do
    let bs = encode a
    putWord32host . fromIntegral $ BL.length bs
    putLazyByteString bs
  get = do
    l <- getWord32host
    bs <- getLazyByteString $ fromIntegral l
    case eitherDecode bs of
      Right a -> pure $ NativeMessage a
      Left err -> fail err

hEncodeAndSend :: (ToJSON a, FromJSON a) => a -> IO ()
hEncodeAndSend a = hPutBuilder stdout . execPut . put $ NativeMessage a

main :: IO ()
main = do
  hSetBuffering stdin $ BlockBuffering Nothing
  hSetBinaryMode stdin True
  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBinaryMode stdout True
  withFile "/Users/fanshi/Personal/chrome-extensions/youtube-mpv/temp.log" WriteMode $ \logH -> do
    let printError err = hPutStrLn logH $ "error: " <> err
        printInfo info = hPutStrLn logH $ "info: " <> info
        printBytes bytes = printInfo $ "Comsumed " <> show bytes <> " bytes."
        sendMPVReply = BS.putStr . toStrict . B.encode . NativeMessage
    hWaitForInput stdin 10000 >>= \case
      False -> sendMPVReply $ MPVError "No Input"
      True -> do
        l <- BL.getContents
        case runGetOrFail get l of
          Left (_, b, err) -> do
            printBytes b
            printError err
            hEncodeAndSend . MPVError $ T.pack err
          Right (_, b, NativeMessage (VideoId vid)) -> do
            printBytes b
            let runMpv = withCreateProcess
                  (shell $ "mpv ytdl://" <> T.unpack vid)
                    { std_in = NoStream,
                      std_out = CreatePipe,
                      std_err = CreatePipe
                    }
                  $ \_ mHout mHErr h -> case (mHout, mHErr) of
                    (Nothing, _) -> pure $ MPVError "no std_out handle for mpv"
                    (_, Nothing) -> pure $ MPVError "no std_err handle for mpv"
                    (Just hOut, Just hErr) -> do
                      hGetContents hOut >>= printInfo
                      hGetContents hErr >>= printError
                      waitForProcess h <&> \case
                        ExitSuccess -> MPVOK
                        oops -> MPVError . T.pack $ show oops
            try runMpv
              >>= \case
                Left (err :: SomeException) -> do
                  printError $ show err
                  hEncodeAndSend $ MPVError . T.pack $ show err
                Right reply -> do
                  case reply of
                    MPVError err -> printError $ T.unpack err
                    MPVOK -> printInfo "Done, all good"
                  hEncodeAndSend reply
