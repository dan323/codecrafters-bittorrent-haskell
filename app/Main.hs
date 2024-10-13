{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson ( encode )
import Data.Char (isDigit)
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad as CM ( when )
import Parser (runDecoder, DecodedValue(ST, INT, LST))
import Data.Bits (Bits(xor))
import qualified Control.Monad.RWS as LB

decodeBencodedValue :: B.ByteString -> DecodedValue
decodeBencodedValue = runDecoder

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
            let jsonValue = encodeValue $ decodeBencodedValue $ B.pack encodedValue
            LB.putStr jsonValue
            putStr "\n"
        _ -> putStrLn $ "Unknown command: " ++ command

encodeValue :: DecodedValue -> LB.ByteString
encodeValue (ST st) = encode $ B.unpack st
encodeValue (INT x) = encode x
encodeValue (LST ls) = encode $ "[" ++ foldr ((\z st -> LB.mconcat [z, "," , st]) . encodeValue) (LB.pack "]") ls
