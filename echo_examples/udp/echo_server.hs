module Main (main) where

import Network.Socket
import Network.BSD
import System (getArgs)

-- This code is inspired by
-- http://book.realworldhaskell.org/read/sockets-and-syslog.html

-- (Sparse) Documentation for the haskell network libraries can be found at
--     http://hackage.haskell.org/package/network-2.3.0.11
-- Look unnder the Modules section

main :: IO()
main = do
  args <- getArgs
  let port = head args
  openSocket port


openSocket :: String -> IO()
openSocket port = do
  -- Resolve host or name
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE], addrSocketType=Datagram}))
    Nothing
    (Just port)
  let serveraddr = head addrinfos
  -- create a socket
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  -- bind the socket to the address
  bindSocket sock (addrAddress serveraddr)
  processMessages sock


processMessages :: Socket -> IO()
processMessages sock = do
  (msg, _, addr) <- recvFrom sock 1024
  printPacket addr msg
  -- send it back to the client
  -- ??
  -- Wait for another message
  processMessages sock


printPacket :: SockAddr -> String -> IO()
printPacket addr msg =
  putStrLn $ "From " ++ show addr ++ ": " ++ msg