{-# LANGUAGE OverloadedStrings #-}
module Torrent (fromDecoded, Torrent(..), TorrentInfo(..)) where

import Parser
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromMaybe)

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