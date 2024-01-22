{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent.Async (concurrently_, race)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson as JSON (KeyValue ((.=)), ToJSON (toEncoding, toJSON), eitherDecode, encode, pairs, withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.Binary as B (Binary (get, put))
import Data.Binary.Get (getLazyByteString, getWord32host, runGetOrFail)
import Data.Binary.Put (execPut, putLazyByteString, putWord32host)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy as BL (ByteString, getContents, length)
import Data.ByteString.Lazy.Char8 as BL (lines)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Text qualified as T (Text, pack, stripPrefix, unpack)
import Data.Text.IO qualified as T
import Network.Socket (Family (AF_UNIX), SocketType (Datagram), close, defaultProtocol, socketPair, withFdSocket)
import Network.Socket.ByteString.Lazy as SOC (getContents, sendAll)
import System.Directory.OsPath (findExecutable)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (BufferMode (BlockBuffering), hFlush, hSetBinaryMode, hSetBuffering, stdin, stdout)
import System.OsPath qualified as FP (decodeUtf)
import System.OsString (osstr)
import System.Process.Typed (createPipe, getStdout, nullStream, proc, setStderr, setStdin, setStdout, waitExitCode, withProcessTerm, withProcessWait)

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

newtype MPVError = MPVError T.Text deriving (Show, Eq)

instance ToJSON MPVError where
  toJSON = undefined
  toEncoding (MPVError err) = pairs $ "error" .= err

newtype MPVInfo = MPVInfo T.Text deriving (Show, Eq)

instance ToJSON MPVInfo where
  toJSON = undefined
  toEncoding (MPVInfo info) = pairs $ "info" .= info

main :: IO ()
main = do
  hSetBuffering stdin $ BlockBuffering Nothing
  hSetBinaryMode stdin True
  hSetBuffering stdout $ BlockBuffering Nothing
  hSetBinaryMode stdout True
  findExecutable [osstr|mpv|] >>= \case
    Nothing -> do
      hEncodeAndSend $ MPVError "mpv not found"
      exitWith $ ExitFailure 127
    Just mpv -> do
      mpv' <- FP.decodeUtf mpv
      let mpvProcess fd =
            setStdin nullStream . setStdout nullStream . setStderr nullStream $
              proc mpv' ["--input-ipc-client=fd://" <> show fd, "--idle", "--keep-open", "--profile=youtube-mpv"]
      findExecutable [osstr|yt-dlp|] >>= \case
        Nothing -> do
          hEncodeAndSend $ MPVError "yt-dlp not found"
          exitWith $ ExitFailure 127
        Just ytdlp -> do
          ytdlp' <- FP.decodeUtf ytdlp
          let youtubeExtractorArgs = intercalate ";" ["comment_sort=top", "max-comments=30,10,20,10", "skip=hls,dash,translated_subs", "player_skip=configs,webpage,js"]
              parseVid = withObject "loadfile-command" $ \v ->
                v .: "command" >>= \case
                  ["loadfile", T.stripPrefix "ytdl://" -> mVid] -> case mVid of
                    Nothing -> fail "not ytdl://"
                    Just vid -> pure vid
                  _ -> fail "no loadfile"
              downdloadCommentsProcess vid =
                setStdin nullStream . setStdout createPipe . setStderr createPipe $
                  proc
                    ytdlp'
                    [ "--write-comments",
                      "--skip-download",
                      "--extractor-args=youtube:" <> youtubeExtractorArgs,
                      "--print=%(comments)j",
                      vid
                    ]
          bracket (socketPair AF_UNIX Datagram defaultProtocol) (\(soc1, soc2) -> close soc1 *> close soc2) $ \(soc1, soc2) -> do
            let runMpvIPC = withFdSocket soc2 $ \fd -> do
                  withProcessTerm (mpvProcess fd) $ \p ->
                    let parseAndSendCommand l = case runGetOrFail get l of
                          Right (uncomsumed, b, NativeMessage cmd) -> do
                            hEncodeAndSend . MPVInfo $ "Comsumed " <> T.pack (show b) <> " bytes."
                            hEncodeAndSend cmd
                            sendAll soc1 cmd
                            sendAll soc1 "\n"
                            parseAndSendCommand uncomsumed
                              `concurrently_` case JSON.eitherDecode cmd >>= parseEither parseVid of
                                Left _err -> pure ()
                                Right vid ->
                                  withProcessWait (downdloadCommentsProcess $ T.unpack vid) $ \(getStdout -> hOut) ->
                                    T.hGetContents hOut >>= hEncodeAndSend
                          Left _ -> pure ()
                        commandLoop = BL.getContents >>= parseAndSendCommand
                     in (MPVError "commadLoop ended before mpv" <$ commandLoop) `race` waitExitCode p
                mpvOutputPrintloop = SOC.getContents soc1 >>= traverse_ ((*> hFlush stdout) . hEncodeAndSend) . BL.lines
            try ((MPVError "mpvOutputPrintloop ended before mpv" <$ mpvOutputPrintloop) `race` runMpvIPC) >>= \case
              Left (err :: SomeException) -> hEncodeAndSend . MPVError . T.pack $ show err
              Right (Right (Right ExitSuccess)) -> hEncodeAndSend $ MPVInfo "Done, all good"
              Right (Right (Right oops)) -> hEncodeAndSend . MPVError . T.pack $ show oops
              Right (Right (Left oops)) -> hEncodeAndSend oops
              Right (Left err) -> hEncodeAndSend err
