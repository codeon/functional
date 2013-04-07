import Control.Concurrent
{-import qualified Control.Exception-} 
import Control.Exception 
import Control.Monad
import Network
import System.IO
import Data.Text
{-import System.IO.Error-}

listenPort :: PortID
listenPort = PortNumber 10020

main :: IO ()
main = do sock <- listenOn listenPort
          acceptLoop sock `finally` sClose sock

acceptLoop :: Socket -> IO ()
acceptLoop sock = forever $ accept sock >>= forkIO . worker

printHere :: String -> IO ()
printHere mesg = do
	putStrLn mesg

sendData :: Handle -> String -> IO ()
sendData hand toBeSend = do
	hPutStrLn hand toBeSend

rcvData :: Handle -> IO String
rcvData hand = do
	dataRecv <- (hGetLine hand)
	return dataRecv

ftpHandler :: Handle -> IO()
ftpHandler hand = do
	x <- rcvData hand
	parse x hand
	ftpHandler hand

parse :: String -> Handle -> IO ()
parse dataRecv hand = case unpack $ strip $ pack dataRecv of
	"cwd" -> sendData hand "Current Directory"
	"pwd" -> sendData hand "Present Directory"
	"get" -> do { x<- rcvData hand ; handle_get (unpack $ strip $ pack x) hand} 
	_ -> do { printHere dataRecv ; sendData hand "SAmajh nahin aaya"}

handle_get :: String -> Handle -> IO ()
handle_get filename hand = 
	do x <- try $ openFile filename ReadMode
	   case x of
		Left er -> sendData hand (show (er::IOException))
		Right x -> hGetContents x >>= sendData hand 

worker :: (Handle, HostName, PortNumber) ->  IO ()
worker (hand, host, port)  = do
  tID <- myThreadId
  putStrLn $ show tID ++ " <- " ++ host ++ ":" ++ show port
  hSetBuffering hand LineBuffering
  ftpHandler hand `finally` do {hClose hand ; putStrLn $ "done " ++ host ++ ":" ++ show port}
