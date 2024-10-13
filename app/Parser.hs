module Parser
    (runDecoder,fromValue)
where

import Data.ByteString.Char8 (ByteString, readInt, unpack, pack)
import qualified Data.ByteString as B
import Text.Megaparsec (Parsec, many, single, satisfy, count, anySingle, parse, choice)
import Text.Megaparsec.Byte (digitChar)
import Data.Void ( Void )
import Data.Functor ( ($>) )
import Data.Char (isDigit, ord)
import GHC.Word (Word8)
import Control.Monad.Identity (Identity)
import Data.Aeson (Value(Bool))
import Data.ByteString.Builder (toLazyByteString)
import Control.Monad.Combinators (option)

type Decoder = Parsec Void ByteString DecodedValue
type Parser = Parsec Void ByteString

data DecodedValue = ST ByteString | INT Int

intParser :: Parser Int
intParser = read . unpack . B.pack <$> many digitChar

stringDecoder :: Decoder
stringDecoder = intParser >>= (\len -> separatorParser *> (ST . B.pack <$> count len anySingle))

valueDecoder :: Decoder
valueDecoder = choice [stringDecoder, intDecoder]

separatorParser :: Parser Word8
separatorParser = single (fromIntegral . ord $ ':'::Word8)

intDecoder :: Decoder
intDecoder = INT <$> (single (fromIntegral . ord $ 'i') *> (option id (single (fromIntegral . ord $ '-') $> negate) <*> intParser) <* single (fromIntegral . ord $ 'e'))

runDecoder :: ByteString -> DecodedValue
runDecoder input = case parse valueDecoder "" input of
    Left x  -> error "oops"
    Right y -> y

fromValue :: DecodedValue -> ByteString
fromValue (ST st) = st
fromValue (INT i) = pack $ show i