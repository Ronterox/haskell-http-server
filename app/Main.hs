{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack, putStrLn, stripPrefix, words)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Prelude hiding (putStrLn, words)

getPath :: ByteString -> ByteString
getPath req = words req !! 1

echo :: ByteString -> ByteString
echo path = fromMaybe "" $ stripPrefix "/echo/" path

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

        let msg = case path of
                _ | "/echo" `isPrefixOf` path -> "HTTP/1.1 200 OK\r\n\r\n" <> echo path <> "\r\n\r\n"
                "/" -> "HTTP/1.1 200 OK\r\n\r\n"
                _ -> "HTTP/1.1 404 Not Found\r\n\r\n"

        print $ path <> " " <> msg
        _ <- send clientSocket msg

        close clientSocket
