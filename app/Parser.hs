{-# LANGUAGE TupleSections #-}
module Parser
    (runDecoder,DecodedValue(ST, INT, LST, DIC))
where

import Data.ByteString.Char8 (readInt, unpack, pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Text.Megaparsec (Parsec, many, single, satisfy, count, anySingle, parse, choice, (<?>),errorBundlePretty)
import Text.Megaparsec.Byte (digitChar)
import Data.Void ( Void )
import Data.Functor ( ($>) )
import Data.Char (isDigit, ord)
import GHC.Word (Word8)
import Control.Monad.Identity (Identity)
import Control.Monad.Combinators (option)

type Decoder = Parsec Void B.ByteString DecodedValue
type Parser = Parsec Void B.ByteString

data DecodedValue = ST BL.ByteString | INT Int | LST [DecodedValue] | DIC [(BL.ByteString, DecodedValue)]

listDecoder :: Decoder
listDecoder = LST <$> (single (fromIntegral . ord $ 'l') *> many valueDecoder <* single (fromIntegral . ord $ 'e'))

mapDecoder :: Decoder
mapDecoder = DIC <$> (single (fromIntegral . ord $ 'd') *> (many (stringDecoder >>= (\(ST key) -> (key,) <$> valueDecoder)) <?> "key - value") <* single (fromIntegral . ord $ 'e'))

intParser :: Parser Int
intParser = read . unpack . B.pack <$> many digitChar

stringDecoder :: Decoder
stringDecoder = intParser >>= (\len -> separatorParser *> (ST . BL.pack <$> count len anySingle))

valueDecoder :: Decoder
valueDecoder = choice [stringDecoder, intDecoder, listDecoder, mapDecoder]

separatorParser :: Parser Word8
separatorParser = single (fromIntegral . ord $ ':'::Word8)

intDecoder :: Decoder
intDecoder = INT <$> (single (fromIntegral . ord $ 'i') *> (option id (single (fromIntegral . ord $ '-') $> negate) <*> intParser) <* single (fromIntegral . ord $ 'e'))

runDecoder :: B.ByteString -> DecodedValue
runDecoder input = case parse valueDecoder "" input of
    Left x  -> error $ errorBundlePretty x
    Right y -> y