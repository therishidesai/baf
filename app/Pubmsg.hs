module Main where

import qualified Data.ByteString as BS

import Control.Exception

import Data.Maybe

import System.IO
import System.IO.Error

import System.Exit
import System.FileLock
import System.Directory
import System.Posix.IO

import Options.Applicative

newtype PubmsgArgs = PubmsgArgs
  { topic :: String }


lockTopic :: String -> IO ()
lockTopic t = do
  lock <- tryLockFile t Exclusive
  if isNothing lock
    then die "Topic already being used"
    else print "locked topic"

parseArgs :: Parser PubmsgArgs
parseArgs = PubmsgArgs
         <$> argument str
             ( metavar "TOPIC"
            <> help "Topic to publish to" )

writeDev :: FilePath -> IO ()
writeDev d = do
  fd <- openFd d ReadWrite Nothing defaultFileFlags
  h <- fdToHandle fd
  hSetBuffering h LineBuffering
  let loop = do
        s <- try $ mapM_ ((BS.hPut h . flip BS.snoc 0xA)  =<<) lines'
        case s of
          Right _ -> loop
          Left e -> do
            if isEOFError e then do
              hPutStrLn stderr "Got EOF"
              hClose h
              pure ()
            else do
              hClose h
              die $ "IOError " ++ show e
  loop

lines' :: [IO BS.ByteString]
lines' = repeat BS.getLine

startPubmsg :: PubmsgArgs -> IO ()
startPubmsg (PubmsgArgs t) = do
                   let t' = "/dev/baf/" ++ t
                   exists <- doesFileExist t'
                   if not exists
                     then die "Topic does not exist"
                     else print "Topic does exist"
                   lockTopic (t' ++ ".lock")
                   writeDev t'

main :: IO ()
main = startPubmsg =<< execParser opts
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Send messages on a TOPIC"
     <> header "pubmsg - a program that will publish data")

                                                        
                                                        
