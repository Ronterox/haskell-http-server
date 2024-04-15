{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString.Char8 (pack, putStrLn)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Prelude hiding (putStrLn)

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

        _ <- recv clientSocket 4096
        _ <- send clientSocket "HTTP/1.1 200 OK\r\n\r\n"

        close clientSocket
