{-# LANGUAGE TupleSections #-}

module Parser (runDecoder, encodeValue, bencode, DecodedValue (..)) where

import Control.Monad.Combinators (option)
import Control.Monad.Identity (Identity)
import Data.Aeson (encode)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, readInt, unpack)
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit, ord)
import Data.Functor (($>))
import Data.List (singleton)
import Data.Map (Map, fromList, toList)
import Data.Void (Void)
import GHC.Word (Word8)
import Text.Megaparsec (Parsec, anySingle, choice, count, errorBundlePretty, many, parse, satisfy, single, (<?>))
import Text.Megaparsec.Byte (digitChar)

type Decoder = Parsec Void B.ByteString DecodedValue

type Parser = Parsec Void B.ByteString

data DecodedValue = ST LB.ByteString | INT Int | LST [DecodedValue] | DIC (Map LB.ByteString DecodedValue)

bencode :: DecodedValue -> LB.ByteString
bencode (ST st) = bencodeString st
bencode (INT x) = bencodeInt x
bencode (LST xs) = LB.concat [LB.pack . pure . fromIntegral . ord $ 'l', LB.concat $ fmap bencode xs, LB.pack . pure . fromIntegral . ord $ 'e']
bencode (DIC d) = LB.concat [LB.pack . pure . fromIntegral . ord $ 'd', LB.concat $ (\(k, v) -> LB.concat [bencodeString k, bencode v]) <$> toList d, LB.pack . pure . fromIntegral . ord $ 'e']

bencodeString :: LB.ByteString -> LB.ByteString
bencodeString st = LB.concat [encode . LB.length $ st, LB.pack . pure . fromIntegral . ord $ ':', st]

bencodeInt :: Int -> LB.ByteString
bencodeInt i = LB.concat [LB.pack . pure . fromIntegral . ord $ 'i', encode i, LB.pack . pure . fromIntegral . ord $ 'e']


listDecoder :: Decoder
listDecoder = LST <$> (single (fromIntegral . ord $ 'l') *> many valueDecoder <* single (fromIntegral . ord $ 'e'))

mapDecoder :: Decoder
mapDecoder = DIC . fromList <$> (single (fromIntegral . ord $ 'd') *> (many (stringDecoder >>= (\(ST key) -> (key,) <$> valueDecoder)) <?> "key - value") <* single (fromIntegral . ord $ 'e'))

intParser :: Parser Int
intParser = read . unpack . B.pack <$> many digitChar

stringDecoder :: Decoder
stringDecoder = intParser >>= (\len -> separatorParser *> (ST . LB.pack <$> count len anySingle))

valueDecoder :: Decoder
valueDecoder = choice [stringDecoder, intDecoder, listDecoder, mapDecoder]

separatorParser :: Parser Word8
separatorParser = single (fromIntegral . ord $ ':' :: Word8)

intDecoder :: Decoder
intDecoder = INT <$> (single (fromIntegral . ord $ 'i') *> (option id (single (fromIntegral . ord $ '-') $> negate) <*> intParser) <* single (fromIntegral . ord $ 'e'))

runDecoder :: B.ByteString -> DecodedValue
runDecoder input = case parse valueDecoder "" input of
  Left x -> error $ errorBundlePretty x
  Right y -> y

encodeValue :: DecodedValue -> LB.ByteString
encodeValue (ST st) = encode $ B.unpack $ LB.toStrict st
encodeValue (INT x) = encode x
encodeValue (DIC ls) = foldObj LB.empty $ toList ls
  where
    foldObj z [] = LB.concat [LB.pack . singleton . fromIntegral . ord $ '{', z, LB.pack . singleton . fromIntegral . ord $ '}']
    foldObj z ((k, v) : xs)
      | z == LB.empty = foldObj (LB.concat [LB.pack . singleton . fromIntegral . ord $ '"', k, LB.pack . singleton . fromIntegral . ord $ '"', LB.pack . singleton . fromIntegral . ord $ ':', encodeValue v]) xs
      | otherwise = foldObj (LB.concat [z, LB.pack . singleton . fromIntegral . ord $ ',', LB.pack . singleton . fromIntegral . ord $ '"', k, LB.pack . singleton . fromIntegral . ord $ '"', LB.pack . singleton . fromIntegral . ord $ ':', encodeValue v]) xs
encodeValue (LST ls) = fold LB.empty $ fmap encodeValue ls
  where
    fold z [] = LB.concat [LB.pack . singleton . fromIntegral . ord $ '[', z, LB.pack . singleton . fromIntegral . ord $ ']']
    fold z (x : xs)
      | z == LB.empty = fold x xs
      | otherwise = fold (LB.concat [z, LB.pack . singleton . fromIntegral . ord $ ',', x]) xs