module Main where

import IO
import Control.Monad (forever)
import System (getArgs)
import Network.BSD
import Network.Socket

data ConnectionHandle = Handle' {hSocket::Socket, hAddress::SockAddr}

main :: IO()
main = do
  args <- getArgs
  let host = head args
  let port = head $ tail args
  handle <- openConnection host port
  writeLoop handle
  sClose $ hSocket handle


openConnection :: HostName -> String -> IO ConnectionHandle
openConnection host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

  return $ Handle' sock (addrAddress serveraddr)

writeLoop :: ConnectionHandle -> IO()
writeLoop (Handle' sock addr) = do
  msg <- getLine
  sendAllTo sock addr msg



-- sendTo only sends the amount of chars equal to the integer it returns
-- so we keep sending until the string is null
sendAllTo :: Socket -> SockAddr -> String -> IO()
sendAllTo _ _ [] = return ()
sendAllTo sock addr msg = do
  numSent <- sendTo sock msg addr
  sendAllTo sock addr (drop numSent msg)

