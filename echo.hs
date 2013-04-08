import Control.Concurrent
{-import qualified Control.Exception-} 
import Control.Exception 
import Control.Monad
import Network
import System.IO
import Data.Text hiding (map)
{-import System.IO.Error-}

data ConnMode = NoChannel
			  |	Active
			  | Passive
			  deriving (Show, Eq)
data DataMode = ASCII
			  | BINARY
			  deriving (Show, Eq)

data FTPState = FTPState
              { logged_in :: Bool, -- check auth status
                datatype :: DataMode, -- ASCII/BINARY
                connmode :: ConnMode, -- Active/Passive
                datasocket :: Handle, -- Handle for sending receiving file data
                cmdsocket :: Handle, -- Handle for sending receiving cmd data
                curr_directory :: String -- Store the current directory
                } 
                
initDefaultFTPState :: Handle -> FTPState
initDefaultFTPState cmdHandle= FTPState
							  { logged_in = False,
								datatype = ASCII,
								connmode = NoChannel,
								cmdsocket = cmdHandle,
								datasocket = cmdHandle,
								curr_directory = "./"
								}

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

ftpHandler :: FTPState -> IO()
ftpHandler ftpState = do
	x <- rcvData (cmdsocket ftpState) -- This line needs to be changed, should use cmdsocket from ftpState instead of directly using hand.
	parse x ftpState
	ftpHandler ftpState

isdelm :: Char -> Bool
isdelm ' ' = True
isdelm  _ = False 

parse :: String -> FTPState ->  IO ()
parse dataRecv ftpState = do
	printHere dataRecv
	let cmd:args = map unpack $ split isdelm $ strip $ pack dataRecv
	case cmd of
		"CWD" -> sendData (cmdsocket ftpState) "Current Directory"
		"PWD" -> sendData (cmdsocket ftpState) "Present Directory"
		"GET" -> do { x<- rcvData (cmdsocket ftpState) ; handle_get (unpack $ strip $ pack x) ftpState} 
		"USER" -> sendData (cmdsocket ftpState) "331 Password?"
		"PORT" -> do {handle_PORT ftpState args}
		"PASS" -> sendData (cmdsocket ftpState) "230 Login Successful !"
		_ -> do { printHere dataRecv ; sendData (cmdsocket ftpState) "SAmajh nahin aaya"}

handle_PORT :: FTPState -> [String] -> IO ()
handle_PORT ftpState args = do
	sendData (cmdsocket ftpState) "Handling PORT Command"


-- Need to add `finally` to close the opened file handle in handle_get
handle_get :: String -> FTPState -> IO ()
handle_get filename ftpState = do
	x <- try $ openFile filename ReadMode
	case x of
		Left er -> sendData (cmdsocket ftpState) (show (er::IOException))
		Right x -> hGetContents x >>= sendData (cmdsocket ftpState) -- this would need to be changed to datasocket once we figure out how to get the datasocket. 
			

worker :: (Handle, HostName, PortNumber) ->  IO ()
worker (hand, host, port)  = do
  tID <- myThreadId
  putStrLn $ show tID ++ " <- " ++ host ++ ":" ++ show port
  hSetBuffering hand LineBuffering
  sendData hand "220 Welcome to FTP Server."
  let ftpState = initDefaultFTPState hand in
	ftpHandler ftpState `finally` do {hClose hand ; putStrLn $ "done " ++ host ++ ":" ++ show port}
