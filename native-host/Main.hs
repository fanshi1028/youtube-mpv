{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson as JSON (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toEncoding, toJSON), encode, pairs)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Binary as B (Binary (get, put), encode)
import Data.Binary.Get (getLazyByteString, getWord32host, runGetOrFail)
import Data.Binary.Put (execPut, putLazyByteString, putWord32host)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy qualified as BL (ByteString, getContents, length, putStr)
import Data.ByteString.Lazy.Char8 as BL (lines)
import Data.Foldable (traverse_)
import Data.Text as T (Text, pack)
import Network.Socket (Family (AF_UNIX), SocketType (Datagram), close, defaultProtocol, socketPair, withFdSocket)
import Network.Socket.ByteString.Lazy as SOC (getContents, sendAll)
import System.Directory.OsPath (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (BlockBuffering, LineBuffering), Handle, hClose, hFlush, hPutStrLn, hSetBinaryMode, hSetBuffering, openTempFile, stdin, stdout)
import System.OsPath as FP (decodeUtf, encodeUtf, (</>))
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (NoStream), interruptProcessGroupOf, shell, waitForProcess, withCreateProcess)

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
      getWord32host >>= getLazyByteString . fromIntegral

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
  withMpvTempFile "log" $ \(_logFile, hLog) -> do
    hSetBuffering hLog LineBuffering
    let printError err = hPutStrLn hLog $ "error: " <> err
        printInfo info = hPutStrLn hLog $ "info: " <> info
    bracket (socketPair AF_UNIX Datagram defaultProtocol) (\(soc1, soc2) -> close soc1 *> close soc2) $ \(soc1, soc2) -> do
      let runMpvIPC = withFdSocket soc2 $ \fd -> do
            let process = (shell $ "mpv --input-ipc-client=fd://" <> show fd <> " --idle --keep-open") {std_in = NoStream, std_out = NoStream, std_err = NoStream}
            withCreateProcess process $ \mHIn mHOut mHErr h -> case (mHIn, mHOut, mHErr) of
              (Just _, _, _) -> pure . Left $ MPVError "impossible: std_in handle exists for mpv"
              (_, Just _, _) -> pure . Left $ MPVError "impossible: std_out handle exists for mpv"
              (_, _, Just _) -> pure . Left $ MPVError "impossible: std_err handle exists for mpv"
              (Nothing, Nothing, Nothing) ->
                Right . fst <$> do
                  let parseAndSendCommand l = case runGetOrFail get l of
                        Right (uncomsumed, b, NativeMessage cmd) -> do
                          let (cps, cmd') = case cmd of
                                "" ->
                                  ( hFlush hLog *> interruptProcessGroupOf h,
                                    encodingToLazyByteString $ pairs ("command" .= ["quit" :: Text])
                                  )
                                _ -> (parseAndSendCommand uncomsumed, cmd)
                          printInfo $ "Comsumed " <> show b <> " bytes."
                          printInfo $ show cmd'
                          sendAll soc1 cmd'
                          sendAll soc1 "\n"
                          cps
                        Left _ -> pure ()
                      commandLoop = BL.getContents >>= parseAndSendCommand
                   in waitForProcess h `concurrently` commandLoop
          mpvOutputPrintloop =
            SOC.getContents soc1
              >>= traverse_ ((*> hFlush stdout) . BL.putStr . B.encode . NativeMessage) . BL.lines
      try (fst <$> runMpvIPC `concurrently` mpvOutputPrintloop) >>= \case
        Left (err :: SomeException) -> do
          printError $ show err
          hEncodeAndSend $ MPVError . T.pack $ show err
        Right (Right ExitSuccess) -> printInfo "Done, all good"
        Right (Right oops) -> hEncodeAndSend . MPVError . T.pack $ show oops
        Right (Left err) -> hEncodeAndSend err
