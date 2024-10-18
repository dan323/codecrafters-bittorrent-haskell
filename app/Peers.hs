{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

module Peers where

import Data.Char (ord)
import GHC.Word (Word8)
import Data.List (intersperse)
import Text.URI (mkURI)
import Data.Aeson (encode)
import Data.ByteString as B (ByteString, unpack, putStr, pack, concat)
import Data.ByteString.Lazy as BL (ByteString, putStr, toStrict, length, unpack, pack, concat)
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Encoding as TT (decodeUtf8, decodeUtf16LE)
import qualified Data.Text.Lazy as T (toStrict, unpack)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import qualified Data.Text as T (Text, pack, concat)
import qualified Data.Text.IO as T (putStrLn)
import Torrent (Torrent (..), TorrentInfo (..), infoHash)
import Parser (runDecoder, DecodedValue(..))
import Text.Printf (printf)
import Data.Map ((!))
import qualified Network.HTTP.Types as H (urlEncodeBuilder)
import Network.HTTP.Req
    ( QueryParam(..), (=:),
      defaultHttpConfig,
      lbsResponse,
      useHttpURI,
      req,
      reqCb,
      runReq,
      GET(GET),
      LbsResponse,
      NoReqBody(NoReqBody),
      Option(..),
      Req,
      Scheme(Http),
      responseBody)
import Web.HttpApiData (ToHttpApiData(..))
import qualified Network.HTTP.Client as L

getPeers :: Torrent -> IO [String]
getPeers torrent = do
    let method = GET
    let body = NoReqBody
    uri <- (mkURI . T.toStrict . T.decodeUtf8 . announce) torrent
    response <- case useHttpURI uri of
                        Just (url, opts) -> do
                            let respType = lbsResponse
                            let optsMerged = mappend (optionsFromTorrent torrent) opts
                            runReq defaultHttpConfig $ reqCb GET url body respType optsMerged $ modifyQuery torrent
                        Nothing -> error "No URL to call"
    return $ toPeers $ runDecoder $ BL.toStrict $ responseBody response

modifyQuery :: Torrent -> L.Request -> Req L.Request
modifyQuery torrent req = do
    let qs = L.queryString req
    let finalReq = req {L.queryString = B.concat [qs, "&info_hash=", B.pack $ (fromIntegral . ord) <$> (toURLEncode $ infoHash torrent) ]}
    return finalReq

optionsFromTorrent :: Torrent -> Option 'Http
optionsFromTorrent torrent = 
            "port" =: (6881 :: Int)
            <> "downloaded" =: (0 :: Int)
            <> "uploaded" =: (0 :: Int)
            <> "compact" =: (1 :: Int)
            <> "left" =: (T.toStrict . T.decodeUtf8 . encode . torrentLength . info $ torrent)
            <> "peer_id" =: ("GCbyiBYUqq7CQqR1wZk4" :: T.Text)


toURLEncode :: B.ByteString -> String
toURLEncode bytes = B.unpack bytes >>= printf "%%%02x"

toPeers :: DecodedValue -> [String]
toPeers (DIC d) = let ls = (d ! "peers") in
                    case ls of
                        (ST value) -> filterIPPort value
                        _          -> error "Oooops!!"


filterIPPort :: BL.ByteString -> [String]
filterIPPort input 
    | BL.length input == 0 = []
    | otherwise = let (ip, portAndRest) =  splitAt 4 $ BL.unpack input in
                    let (port, rest) = splitAt 2 portAndRest in
                        let ipFinal = Prelude.concat $ intersperse "." $ ((show . toInteger) <$> ip) in
                            let portFinal = show $ (toInteger $ head port)*16*16 + (toInteger . head . tail $ port) in
                                (Prelude.concat [ipFinal, ":", portFinal]) : (filterIPPort $ BL.pack rest)