{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString.Char8 (ByteString, isPrefixOf, length, lines, pack, putStrLn, stripPrefix, words)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Prelude hiding (length, lines, putStrLn, words)

getPath :: ByteString -> ByteString
getPath req = words req !! 1

echo :: ByteString -> ByteString
echo path = fromMaybe "" $ stripPrefix "/echo/" path

userAgent :: ByteString -> ByteString
userAgent req = lines (fromMaybe "" $ stripPrefix "User-Agent: " req) !! 1

contentLength :: ByteString -> ByteString
contentLength body = pack $ show $ length body

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    putStrLn "Logs from your program will appear here"

    let host = "127.0.0.1"
        port = "4221"

    putStrLn $ "Listening on http://" <> pack host <> ":" <> pack port

    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
    setSocketOption serverSocket ReuseAddr 1

    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5

    -- Accept connections and handle them forever
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        putStrLn $ "Accepted connection from " <> pack (show clientAddr) <> "."
        -- Handle the clientSocket as needed...

        req <- recv clientSocket 4096
        let path = getPath req
            okmsg = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: "
            notFound = "HTTP/1.1 404 Not Found\r\n\r\n"
            eof = "\r\n\r\n"

        let msg = case path of
                _ | "/echo" `isPrefixOf` path -> okmsg <> contentLength body <> eof <> body <> eof
                  where
                    body = echo path
                "/user-agent" -> okmsg <> contentLength agent <> eof <> agent <> eof
                  where
                    agent = userAgent req
                "/" -> okmsg <> "13\r\n\r\nHello, World!\r\n\r\n"
                _ -> notFound

        print $ path <> " " <> msg
        _ <- send clientSocket msg

        close clientSocket
