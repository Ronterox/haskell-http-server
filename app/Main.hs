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

getField :: ByteString -> ByteString -> ByteString
getField field content = fromMaybe "" $ stripPrefix field content

echo :: ByteString -> ByteString
echo path = getField "/echo/" path

userAgent :: ByteString -> ByteString
userAgent req = findAgent $ lines req
  where
    findAgent [] = ""
    findAgent (line : rest)
        | "User-Agent:" `isPrefixOf` line = getField "User-Agent: " line
        | otherwise = findAgent rest

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
            msgOk = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: "
            msgNotFound = "HTTP/1.1 404 Not Found\r\n\r\n"
            eof = "\r\n\r\n"

        let msg = case path of
                _ | "/echo" `isPrefixOf` path -> msgOk <> contentLength body <> eof <> body <> eof
                  where
                    body = echo path
                "/user-agent" -> msgOk <> contentLength agent <> eof <> agent <> eof
                  where
                    agent = userAgent req
                "/" -> msgOk <> "13\r\n\r\nHello, World!\r\n\r\n"
                _ -> msgNotFound

        print $ path <> " " <> msg
        _ <- send clientSocket msg

        close clientSocket
