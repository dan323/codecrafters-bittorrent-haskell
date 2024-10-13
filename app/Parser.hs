module Parser
    (runDecoder,fromValue)
where

import Data.ByteString.Char8 (ByteString, readInt, unpack, pack)
import qualified Data.ByteString as B
import Text.Megaparsec (Parsec, many, single, satisfy, count, anySingle, parse)
import Text.Megaparsec.Byte (digitChar)
import Data.Void
import Data.Char (isDigit, ord)
import GHC.Word (Word8)
import Control.Monad.Identity (Identity)
import Data.Aeson (Value(Bool))
import Data.ByteString.Builder (toLazyByteString)

type Decoder = Parsec Void ByteString DecodedValue
type Parser = Parsec Void ByteString

data DecodedValue = ST ByteString | INT Int

intDecoder :: Parser Int
intDecoder = read . unpack . B.pack <$> many digitChar

stringDecoder :: Decoder
stringDecoder = intDecoder >>= (\len -> separatorDecoder *> (ST . B.pack <$> count len anySingle))

valueDecoder :: Decoder
valueDecoder = stringDecoder

separatorDecoder :: Parser Word8
separatorDecoder = single (fromIntegral . ord $ ':')

runDecoder :: ByteString -> DecodedValue
runDecoder input = case parse valueDecoder "" input of
    Left x  -> error "oops"
    Right y -> y

fromValue :: DecodedValue -> ByteString
fromValue (ST st) = st
fromValue (INT i) = pack $ show i