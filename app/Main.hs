{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson ( encode )
import Data.Char (ord)
import Data.List (singleton)
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad as CM ( when )
import Parser (runDecoder, DecodedValue(ST, INT, LST, DIC))
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
encodeValue (ST st) = encode $ B.unpack $ LB.toStrict st
encodeValue (INT x) = encode x
encodeValue (DIC ls) = foldObj LB.mempty ls
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