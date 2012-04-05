
-- listen, communicate on a port
import Network (listenOn, withSocketsDo, accept, sClose, PortID(..), Socket)
import System (getArgs)

import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
-- to create new haskell threads
import Control.Concurrent (forkIO)

main :: IO()
main = do
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  sock <- listenOn $ PortNumber port
  putStrLn $ "Listening on " ++ (head args)
  return ()
  sockHandler sock
  sClose sock


-- When a new connection is made, spin off a new thread and process with
-- 'commandProcessor'.
sockHandler :: Socket -> IO ()
sockHandler sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ commandProcessor handle
  sockHandler sock


-- Process the input
commandProcessor :: Handle -> IO ()
commandProcessor handle = do
  line <- hGetLine handle
  putStrLn line
  let cmd = words line
  case (head cmd) of
    "echo" -> echoCommand handle cmd
    "add"  -> addCommand handle cmd
    _      -> do hPutStrLn handle "Unknown command"
  commandProcessor handle


echoCommand :: Handle -> [String] -> IO()
echoCommand handle cmd = do
  hPutStrLn handle (unwords $ tail cmd)

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd = do
  hPutStrLn handle $ show $ (read $ cmd !! 1) + (read $ cmd !! 2)