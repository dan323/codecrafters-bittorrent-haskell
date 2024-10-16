{-# LANGUAGE OverloadedStrings #-}
module Torrent (fromDecoded, Torrent(..), infoTorrent) where

import Parser (DecodedValue(..), runDecoder, bencode)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text, concat, pack)
import Data.Text.Encoding         as T  (decodeUtf8)
import Data.List (intercalate)
import Crypto.Hash.SHA1 (hashlazy, hash)

import Text.Printf (printf)
import Data.Aeson ( encode )

data TorrentInfo = Info {
      torrentLength :: Int
    , name :: LB.ByteString
    , pieceLength :: Int
    , pieces :: LB.ByteString
}

data Torrent = Torrent {
      announce :: LB.ByteString
    , info :: TorrentInfo
}

fromDecoded :: DecodedValue -> Torrent
fromDecoded (DIC m) = let ST url = m ! "announce" in 
                        let DIC infoMap = m ! "info" in 
                            let INT len = infoMap ! "length" in 
                                Torrent {
                                    announce = url,
                                    info = Info {
                                        torrentLength = len,
                                        name = case M.lookup "name" infoMap of
                                            Just (ST name) -> name
                                            _              -> LB.empty,
                                        pieceLength = case M.lookup "piece length" infoMap of
                                            Just (INT x) -> x
                                            _ -> 0,
                                        pieces = case M.lookup "pieces" infoMap of
                                            Just (ST x) -> x
                                            _ -> LB.empty
                                    }
                                }
fromDecoded _ = error "Unexpected value"

toDecoded :: Torrent -> DecodedValue
toDecoded (Torrent {announce = ann, info = inf}) = let Info {name = nam, torrentLength = len, pieces = ps, pieceLength = plen} = inf in
        DIC . M.fromList $ [("announce", ST ann), ("info", DIC $ infoMap nam len ps plen)]
    where
        addName n xs = ("name", ST n):xs 
        addPieces ps xs = ("pieces",  ST ps):xs
        addPLength plen xs = ("piece length", INT plen):xs 
        addLength len xs = ("length", INT len):xs
        infoMap nam len ps plen = M.fromList . addLength len . addName nam . addPieces ps . addPLength plen $ []

infoTorrent :: B.ByteString -> T.Text
infoTorrent content = let decodedValue = runDecoder content in
            let torrent = fromDecoded decodedValue in
            let DIC torValue = decodedValue in
            let infoValue = torValue ! "info" in
            let ps = LB.toStrict . pieces . info $ torrent in
            let sha = toHex <$> splitEqual ps 20 in
            let shaText = intercalate "\n" sha in
            T.concat ["Tracker URL: ",T.decodeUtf8 . B.concat . LB.toChunks $ announce torrent, "\n", 
                "Length: ", T.decodeUtf8 . B.concat . LB.toChunks .  encode . torrentLength . info $ torrent, "\n",
                "Info Hash: ", T.pack $ toHex (hashlazy . bencode $ infoValue), "\n",
                "Piece Length: ", T.pack . show . pieceLength . info $ torrent, "\n",
                "Piece Hashes:\n", T.pack shaText]

splitEqual :: B.ByteString -> Int -> [B.ByteString]
splitEqual bs n 
    | bs == B.empty = []
    | otherwise = let (a,b) = B.splitAt n bs in a:splitEqual b n

toHex :: B.ByteString -> String
toHex bytes = B.unpack bytes >>= printf "%02x"