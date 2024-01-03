{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, bracket, try)
import Data.Aeson as JSON (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toEncoding, toJSON), encode, pairs)
import Data.Binary as B (Binary (get, put), encode)
import Data.Binary.Get (getLazyByteString, getWord32host, runGetOrFail)
import Data.Binary.Put (execPut, putLazyByteString, putWord32host)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy qualified as BL (ByteString, getContents, length, putStr)
import Data.ByteString.Lazy.Char8 as BL (lines)
import Data.Foldable (traverse_)
import Data.Text as T (Text, pack)
import Network.Socket (Family (AF_UNIX), SocketType (Datagram), close, defaultProtocol, socketPair, withFdSocket)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import System.Directory.OsPath (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hFlush, hPutStrLn, hSetBinaryMode, hSetBuffering, openTempFile, stdin, stdout)
import System.OsPath as FP (decodeUtf, encodeUtf, (</>))
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (NoStream), shell, waitForProcess, withCreateProcess)

newtype NativeMessage a = NativeMessage a deriving (Show, Eq, ToJSON)

newtype MPVError = MPVError Text deriving (Show, Eq)

instance FromJSON MPVError where
  parseJSON = undefined

instance ToJSON MPVError where
  toJSON = undefined
  toEncoding (MPVError err) = pairs $ "error" .= err

instance Binary (NativeMessage MPVError) where
  put (NativeMessage err) = put $ JSON.encode err
  get = undefined

instance Binary (NativeMessage BL.ByteString) where
  put (NativeMessage bs) = do
    putWord32host . fromIntegral $ BL.length bs
    putLazyByteString bs
  get =
    NativeMessage <$> do
      l <- getWord32host
      getLazyByteString $ fromIntegral l

hEncodeAndSend :: (Binary (NativeMessage a)) => a -> IO ()
hEncodeAndSend a = hPutBuilder stdout . execPut . put $ NativeMessage a

withMpvTempFile :: String -> ((FilePath, Handle) -> IO b) -> IO b
withMpvTempFile name io = do
  dataDir <- (</>) <$> getHomeDirectory <*> FP.encodeUtf ".youtube-mpv-chrome-extension"
  createDirectoryIfMissing False dataDir
  dataDir' <- FP.decodeUtf dataDir
  bracket
    (openTempFile dataDir' name)
    (hClose . snd)
    io

main :: IO ()
main = do
  hSetBuffering stdin $ BlockBuffering Nothing
  hSetBinaryMode stdin True
  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBinaryMode stdout True
  withMpvTempFile "log" $ \(_logFile, logH) -> do
    let printError err = hPutStrLn logH $ "error: " <> err
        printInfo info = hPutStrLn logH $ "info: " <> info
    bracket
      (socketPair AF_UNIX Datagram defaultProtocol)
      (\(soc1, soc2) -> close soc1 *> close soc2)
      $ \(soc1, soc2) ->
        withFdSocket soc2 $ \fd -> do
          let cmd = "mpv --input-ipc-client=fd://" <> show fd <> " --idle --keep-open"
              runMpv cps = withCreateProcess (shell cmd) {std_in = NoStream, std_out = NoStream, std_err = NoStream} $ \mHin mHout mHErr h ->
                case (mHin, mHout, mHErr) of
                  (Nothing, Nothing, Nothing) -> cps h
                  (Just _, _, _) -> pure . Left $ MPVError "impossible: std_in handle exists for mpv"
                  (_, Just _, _) -> pure . Left $ MPVError "impossible: std_out handle exists for mpv"
                  (_, _, Just _) -> pure . Left $ MPVError "impossible: std_err handle exists for mpv"
          done <- try . runMpv $ \h -> do
            let parseAndSendCommand l = case runGetOrFail get l of
                  Right (uncomsumed, b, NativeMessage cmd') -> do
                    printInfo $ "Comsumed " <> show b <> " bytes."
                    printInfo $ show cmd'
                    sendAll soc1 $ cmd' <> "\n"
                    parseAndSendCommand uncomsumed
                  Left _ -> pure ()
            BL.getContents >>= parseAndSendCommand
            recv soc1 4096 >>= traverse_ (BL.putStr . B.encode . NativeMessage) . BL.lines
            Right <$> waitForProcess h
          case done of
            Left (err :: SomeException) -> do
              printError $ show err
              hEncodeAndSend $ MPVError . T.pack $ show err
            Right (Right ExitSuccess) -> printInfo "Done, all good"
            Right (Right oops) -> hEncodeAndSend . MPVError . T.pack $ show oops
            Right (Left err) -> hEncodeAndSend err
