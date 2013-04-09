import Control.Concurrent
{-import qualified Control.Exception-} 
import Control.Exception 
import Control.Monad
import Network
import System.IO
import Data.Text hiding (map)
import Network.Socket hiding (accept)
import Network.BSD (getProtocolNumber)
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
                datasocket :: Socket, -- socket for sending receiving file data
                cmdsocket :: Handle, -- Handle for sending receiving cmd data
                datahandle :: Handle, -- Handle for sending receiving file data
                curr_directory :: String -- Store the current directory
                } 
                
initDefaultFTPState :: Handle  -> Socket ->FTPState
initDefaultFTPState cmdHandle sock = FTPState
							  { logged_in = False,
								datatype = ASCII,
								connmode = NoChannel,
								cmdsocket = cmdHandle,
								datahandle = cmdHandle,
								datasocket = sock,
								curr_directory = "./"
								}

listenPort :: PortID
listenPort = PortNumber 10020
dataPort :: PortID
dataPort = PortNumber 10021

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
	y <- parse x ftpState
	ftpHandler $ y

isdelm :: Char -> Bool
isdelm ' ' = True
isdelm  _ = False 

parse :: String -> FTPState ->  IO FTPState
parse dataRecv ftpState = do
	printHere dataRecv
	let cmd:args = map unpack $ split isdelm $ strip $ pack dataRecv -- splitting the commands received on ' ' . The first word of recvData is the command and rest are arguments.
	case cmd of
		"CWD" -> do 
					sendData (cmdsocket ftpState) "Current Directory" 
					return ftpState
		"PWD" -> do
					sendData (cmdsocket ftpState) "Present Directory"
					return ftpState
		"RETR" -> do 
					sendData (cmdsocket ftpState) "150 send by cmd?"
					printHere $ show $ datahandle ftpState
					handle_get x ftpState 
					--sendData (datahandle ftpState) "150 send by data?"
					hClose (datahandle ftpState)
					sClose (datasocket ftpState)
					sendData (cmdsocket ftpState) "226 Transfer Comp"
					return ftpState			
				where x:xs = args		
		"USER" -> do
					sendData (cmdsocket ftpState) "331 Password?"
					return ftpState
		"LIST" -> do 
					sendData (cmdsocket ftpState) "150 send by cmd?"
					printHere $ show $ datahandle ftpState
					handle_get "echo.hs" ftpState 
					--sendData (datahandle ftpState) "150 send by data?"
					hClose (datahandle ftpState)
					sClose (datasocket ftpState)
					sendData (cmdsocket ftpState) "226 Transfer Comp"
					return ftpState					
--		"PORT" -> do {handle_PORT ftpState args}
--				 ftpState
--		"EPRT" -> do {handle_PORT ftpState args}
--				  ftpState
		"PASS" -> do
					sendData (cmdsocket ftpState) "230 Login Successful !"
					return ftpState
		"PASV" -> handle_PASV ftpState
		"EPSV" -> handle_PASV ftpState
		_ -> do { printHere dataRecv ; sendData (cmdsocket ftpState) "SAmajh nahin aaya" ; return ftpState}
--		_ -> retunr ftpState

handle_PASV :: FTPState -> IO FTPState
handle_PASV ftpState= do 
	sock <- listenOn dataPort
	sendData (cmdsocket ftpState) "227 Enteng Passive Mode (172,27,19,56,39,37)."
	(hand, host, port ) <- accept sock
	printHere $ show hand ++ ":" ++ show host ++ ":" ++ show port
	return $ ftpState {datahandle = hand , datasocket = sock}

handle_PORT :: FTPState -> [String] -> IO ()
handle_PORT ftpState args = do
	sendData (cmdsocket ftpState) "Handling PORT Command"


-- Need to add `finally` to close the opened file handle in handle_get
handle_get :: String -> FTPState -> IO ()
handle_get filename ftpState = do
	x <- try $ openFile filename ReadMode
	case x of
		Left er -> sendData (datahandle ftpState) (show (er::IOException))
		Right x -> hGetContents x >>= sendData (datahandle ftpState) -- this would need to be changed to datahandle once we figure out how to get the datahandle. 
			

worker :: (Handle, HostName, PortNumber) ->  IO ()
worker (hand, host, port)  = do
  tID <- myThreadId
  putStrLn $ show tID ++ " <- " ++ host ++ ":" ++ show port
  hSetBuffering hand LineBuffering
  sendData hand "220 Welcome to FTP Server."
  proto <- getProtocolNumber "tcp"
  sock <- socket AF_INET Stream proto
  let ftpState = initDefaultFTPState hand sock  in
	ftpHandler ftpState `finally` do {hClose hand ; putStrLn $ "done " ++ host ++ ":" ++ show port}
