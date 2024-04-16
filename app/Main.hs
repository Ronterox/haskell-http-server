{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (filterM, forever)
import Data.ByteString.Char8 (ByteString, isPrefixOf, length, lines, pack, putStrLn, strip, stripPrefix, unpack, words)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.Directory (doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Prelude hiding (length, lines, putStrLn, words)

type ContentType = ByteString
type Request = ByteString
type Field = ByteString
type Content = ByteString
type ContentLength = ByteString
type Response = ByteString
type UrlPath = ByteString

getPath :: Request -> UrlPath
getPath req
    | length req > 1 = words req !! 1
    | otherwise = "/"

getOk :: ContentType -> Content -> Response
getOk contentType content = "HTTP/1.1 200 OK\r\nContent-Type: " <> contentType <> "\r\nContent-Length: " <> contentLength content <> eof <> content <> eof
  where
    eof = "\r\n\r\n"

getField :: Field -> Content -> Content
getField field content = strip $ fromMaybe "" $ stripPrefix field content

echo :: UrlPath -> Content
echo = getField "/echo/"

userAgent :: Request -> Content
userAgent req = findAgent $ lines req
  where
    findAgent [] = ""
    findAgent (line : rest)
        | "User-Agent:" `isPrefixOf` line = getField "User-Agent:" line
        | otherwise = findAgent rest

contentLength :: Content -> ContentLength
contentLength body = pack $ show $ length body

handleClient :: Socket -> FilePath -> [FilePath] -> IO ()
handleClient clientSocket directory files = do
    req <- recv clientSocket 4096

    let path = getPath req
    msg <- case path of
        _ | "/files" `isPrefixOf` path && directory ++ filename `elem` files -> do
            body <- readFile $ directory <> filename
            return $ getOk "application/octet-stream" $ pack body
          where
            filename = unpack $ getField "/files/" path
        _ | "/echo" `isPrefixOf` path -> return $ getOk "text/plain" $ echo path
        "/user-agent" -> return $ getOk "text/plain" $ userAgent req
        "/" -> return "HTTP/1.1 200 OK\r\n\r\n"
        _ -> return "HTTP/1.1 404 Not Found\r\n\r\n"

    _ <- send clientSocket msg
    close clientSocket

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    args <- getArgs
    let directory = case args of
            xs
                | "--directory" `elem` xs ->
                    if last dirname == '/'
                        then dirname
                        else dirname <> "/"
              where
                dirname = dropWhile (/= "--directory") xs !! 1
            _ -> "./"

    contents <- getDirectoryContents directory
    files <- filterM doesFileExist $ map (directory <>) contents
    print files

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
        forkIO $ handleClient clientSocket directory files
