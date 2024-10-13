{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit)
import System.Environment
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad as CM
import Parser (runDecoder, fromValue)

decodeBencodedValue :: ByteString -> ByteString
decodeBencodedValue = fromValue . runDecoder

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
            let decodedValue = decodeBencodedValue $ B.pack encodedValue
            let jsonValue = encode $ B.unpack decodedValue
            LB.putStr jsonValue
            putStr "\n"
        _ -> putStrLn $ "Unknown command: " ++ command
