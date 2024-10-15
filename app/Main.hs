{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ( encode )
import Crypto.Hash.SHA1 (hashlazy, hash)
import Data.Char (ord)
import Data.List (singleton)
import Data.Map ((!), toList)
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad as CM ( when )
import Parser (runDecoder, DecodedValue(ST, INT, LST, DIC), bencode)
import Torrent (fromDecoded, Torrent(..), TorrentInfo(..))
import qualified Control.Monad.RWS as LB
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    CM.when (length args < 2)
         do putStrLn "Usage: your_bittorrent.sh <command> <args>"
            exitWith (ExitFailure 1)

    let command = head args
    case command of
        "decode" -> do
            -- You can use print statements as follows for debugging, they'll be visible when running tests.
            -- putStrLn "Logs from your program will appear here!"
            -- Uncomment this block to pass stage 1
            let encodedValue = args !! 1
            let jsonValue = encodeValue $ runDecoder $ B.pack encodedValue
            LB.putStr jsonValue
            putStr "\n"
        "info"   -> do
            let filePath = args !! 1
            contents <- B.readFile filePath
            let decodedValue = runDecoder contents
            let torrent = fromDecoded decodedValue
            let DIC torValue = decodedValue
            let infoValue = torValue ! "info"
            LB.putStr $ LB.concat ["Tracker URL: ", announce torrent]
            putStr "\n"
            LB.putStr $ LB.concat ["Length: ", encode . torrentLength . info $ torrent]
            putStr "\n"
            putStr $ "Info Hash: " ++ toHex (hashlazy . bencode $ infoValue)
            putStr "\n"
        _ -> putStrLn $ "Unknown command: " ++ command

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"

encodeValue :: DecodedValue -> LB.ByteString
encodeValue (ST st) = encode $ B.unpack $ LB.toStrict st
encodeValue (INT x) = encode x
encodeValue (DIC ls) = foldObj LB.mempty $ toList ls
    where
        foldObj z     [] = LB.concat [LB.pack . singleton. fromIntegral . ord $ '{', z, LB.pack .  singleton. fromIntegral . ord $ '}']
        foldObj z ((k,v):xs) 
            | z == LB.mempty = foldObj (LB.concat [LB.pack . singleton. fromIntegral . ord $ '"',k,LB.pack . singleton. fromIntegral . ord $ '"',LB.pack . singleton. fromIntegral . ord $ ':', encodeValue v]) xs
            | otherwise      = foldObj (LB.concat [z, LB.pack . singleton. fromIntegral . ord $ ',',LB.pack . singleton. fromIntegral . ord $ '"', k,LB.pack . singleton. fromIntegral . ord $ '"', LB.pack . singleton. fromIntegral . ord $ ':', encodeValue v]) xs
encodeValue (LST ls) = fold LB.mempty $ fmap encodeValue ls
    where
        fold z     [] = LB.concat [LB.pack . singleton. fromIntegral . ord $ '[', z, LB.pack .  singleton. fromIntegral . ord $ ']']
        fold z (x:xs) 
            | z == LB.mempty = fold x xs
            | otherwise      = fold (LB.concat [z, LB.pack . singleton. fromIntegral . ord $ ',', x]) xs