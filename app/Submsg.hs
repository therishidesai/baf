module Main where

import System.IO
import System.Directory
import System.Posix.IO
import System.Posix.Files

import Data.Text as T
import Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

import Options.Applicative

import Prelude as P

newtype SubmsgArgs = SubmsgArgs
  { topics :: Maybe String }


subWorker :: Handle -> TChan BS.ByteString -> IO ()
subWorker h c = forever $ do
  -- TODO: ByteString Lazy for performance?
  l <- BS.hGetLine h
  atomically $ writeTChan c l

parseArgs :: Parser SubmsgArgs
parseArgs = SubmsgArgs
         <$> optional (argument str
             ( metavar "TOPICS"
             <> help "Topics to subscribe to, if none given then all topics will be used. Use a comma separated string" ))

getSubs :: Maybe String -> IO [FilePath]
getSubs (Just t) = do
  let ts = BS.pack t
  d <- listDirectory "/dev/baf"
  let ds = P.map BS.pack d
  let s = P.filter (\l -> BS.length l > 0 && P.elem l ds) $ P.map BS.strip $ BS.split ',' ts
  if P.null s
    then error "No valid topics"
  else
    return (P.map (T.unpack . TE.decodeUtf8) s)

  
getSubs Nothing = do
  listDirectory "/dev/baf"

startSubs :: FilePath -> TChan BS.ByteString -> IO ()
startSubs f c = do
  fd <- openFd f ReadOnly Nothing defaultFileFlags
  h <- fdToHandle fd
  hSetBuffering h LineBuffering
  _ <- forkIO $ subWorker h c
  return ()

startSubmsg :: SubmsgArgs -> IO ()
startSubmsg (SubmsgArgs t) = do
  s <- getSubs t
  c <- newTChanIO
  forM_ s  $ \x -> do
    _ <- forkIO $ startSubs ("/dev/baf/" ++ x) c
    return ()
  forever $ do
    l <- atomically $ readTChan c
    BS.putStrLn l

main :: IO ()
main = startSubmsg =<< execParser opts
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Receive message on TOPICS or all topics"
     <> header "submsg - a program that will subscribe to data")
