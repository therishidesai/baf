module Main where

import System.IO
import System.Directory
import System.Posix.IO
import System.Posix.Files

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

import Options.Applicative

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
  -- TODO: parse out subs that don't match t
  listDirectory "/dev/baf/"
  
getSubs Nothing = do
  listDirectory "/dev/baf/"

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
