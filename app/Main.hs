{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ( encode )
import Data.Char (ord)
import Data.List (singleton, intercalate)
import Data.Map ((!), toList)
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad as CM ( when )
import Parser (encodeValue, runDecoder)
import Torrent (fromDecoded, Torrent(..), infoTorrent)
import qualified Control.Monad.RWS as LB
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (putStr)

main :: IO ()
main = do
    args <- getArgs
    CM.when (length args < 2)
         do putStrLn "Usage: your_bittorrent.sh <command> <args>"
            exitWith (ExitFailure 1)

    let command = head args
    case command of
        "decode" -> do
            let encodedValue = args !! 1
            let jsonValue = decodeCommand encodedValue
            LB.putStr jsonValue
            putStr "\n"
        "info"   -> do
            let filePath = args !! 1
            contents <- B.readFile filePath
            T.putStr $ infoCommand contents
        _ -> putStrLn $ "Unknown command: " ++ command

infoCommand :: B.ByteString -> T.Text
infoCommand = infoTorrent

decodeCommand :: String -> LB.ByteString
decodeCommand = encodeValue . runDecoder . B.pack