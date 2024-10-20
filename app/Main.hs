{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad as CM (when)
import qualified Control.Monad.RWS as LB
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (ord)
import Data.List (intercalate, singleton)
import Data.List.Split (splitOn)
import Data.Map (toList, (!))
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (putStr)
import Parser (encodeValue, runDecoder)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Torrent (Torrent (..), fromDecoded, infoTorrent)
import Peers ( getPeers, handShakePeer )

main :: IO ()
main = do
  args <- getArgs
  CM.when
    (length args < 2)
    do
      putStrLn "Usage: your_bittorrent.sh <command> <args>"
      exitWith (ExitFailure 1)

  let command = head args
  case command of
    "decode" -> do
      let encodedValue = args !! 1
      let jsonValue = decodeCommand encodedValue
      LB.putStr jsonValue
      putStr "\n"
    "info" -> do
      let filePath = args !! 1
      contents <- B.readFile filePath
      T.putStr $ infoCommand contents
    "peers" -> do
      let filePath = args !! 1
      contents <- B.readFile filePath
      let torrent = fromDecoded . runDecoder $ contents
      res <- getPeers torrent
      putStr $ intercalate "\n" res
    "handshake" -> do
      let filePath = args !! 1
      let ipPort = args !! 2
      let ip:port:xs = splitOn ":" ipPort
      contents <- B.readFile filePath
      let torrent = fromDecoded . runDecoder $ contents
      res <- handShakePeer torrent ip port
      B.putStr $ "Peer ID: " <> res
    _ -> putStrLn $ "Unknown command: " ++ command

infoCommand :: B.ByteString -> T.Text
infoCommand = infoTorrent

decodeCommand :: String -> LB.ByteString
decodeCommand = encodeValue . runDecoder . B.pack