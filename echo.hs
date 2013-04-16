import System.Directory
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
                curr_directory :: IO FilePath, -- Store the current directory
                renamefile :: FilePath
                } 
                
initDefaultFTPState :: Handle  -> Socket ->FTPState
initDefaultFTPState cmdHandle sock = FTPState
							  { logged_in = False,
								datatype = ASCII,
								connmode = NoChannel,
								cmdsocket = cmdHandle,
								datahandle = cmdHandle,
								datasocket = sock,
								curr_directory = getCurrentDirectory,
								renamefile = ""
								}

listenPort :: PortID
listenPort = PortNumber 10020
dataPort :: PortID
dataPort = PortNumber 10021

main :: IO ()
main = do 
		sock <- listenOn listenPort
		sock2 <- listenOn dataPort
		acceptLoop sock sock2 `finally` sClose sock

acceptLoop :: Socket -> Socket -> IO ()
acceptLoop sock sock2 = forever $ accept sock >>= forkIO . (worker sock2)

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
					--sClose (datasocket ftpState)
					sendData (cmdsocket ftpState) "226 Transfer Comp"
					return ftpState			
				where x:xs = args		
		"USER" -> do
					sendData (cmdsocket ftpState) "331 Password?"
					return ftpState
		"LIST" -> do 
					sendData (cmdsocket ftpState) "150 send by cmd?"
					printHere $ show $ datahandle ftpState
					handle_ls ftpState 
					hClose (datahandle ftpState)
					sendData (cmdsocket ftpState) "226 Directory Send OK"
					return ftpState					
--		"PORT" -> do {handle_PORT ftpState args}
--				 ftpState
--		"EPRT" -> do {handle_PORT ftpState args}
--				  ftpState
		"PASS" -> do
					sendData (cmdsocket ftpState) "230 Login Successful !"
					return ftpState
		"STOR" -> do
					sendData (cmdsocket ftpState) "150 send me file"
					printHere $ show $ datahandle ftpState
					handle_put x ftpState 
					--sendData (datahandle ftpState) "150 send by data?"
					hClose (datahandle ftpState)
					--sClose (datasocket ftpState)
					sendData (cmdsocket ftpState) "226 Transfer Comp"
					return ftpState			
				where x:xs = args		
		"DELE" -> do
				    --sendData (cmdsocket ftpState) "150 send me file to delete"
				  --printHere $ show $ datahandle ftpState
				    handle_del x ftpState 
					--sendData (datahandle ftpState) "150 send by data?"
				    --hClose (datahandle ftpState)
					--sClose (datasocket ftpState)
				    sendData (cmdsocket ftpState) "250 File deleted"
				    return ftpState			
				where x:xs = args
		"RNFR" -> do
					--return $ ftpState {renamefile = x} 
					sendData (cmdsocket ftpState) "350 Ready for RNTO"
					--printHere $ show $ renamefile ftpState
					return $ ftpState {renamefile = x}
				where x:xs = args
		"RNTO" -> do
					--sendData (cmdsocket ftpState) "150 send me file to rename"
					printHere $ show $ renamefile ftpState
					handle_rename x ftpState 
					--sendData (datahandle ftpState) "150 send by data?"
					--hClose (datahandle ftpState)
					--sClose (datasocket ftpState)
					--sendData (cmdsocket ftpState) "250 Rename Successful"
					return ftpState			
				where x:xs = args
		"PASV" -> handle_PASV ftpState
		"EPSV" -> handle_PASV ftpState
		_ -> do { printHere dataRecv ; sendData (cmdsocket ftpState) "SAmajh nahin aaya" ; return ftpState}
--		_ -> retunr ftpState

handle_PASV :: FTPState -> IO FTPState
handle_PASV ftpState= do 
	--sock <- listenOn dataPort
	sendData (cmdsocket ftpState) "227 Entering Passive Mode (172,27,22,118,39,37)."
	(hand, host, port ) <- accept (datasocket ftpState)
	printHere $ show hand ++ ":" ++ show host ++ ":" ++ show port
	return $ ftpState {datahandle = hand }

handle_PORT :: FTPState -> [String] -> IO ()
handle_PORT ftpState args = do
	sendData (cmdsocket ftpState) "Handling PORT Command"

handle_ls :: FTPState -> IO ()
handle_ls ftpState = do
	y <- curr_directory ftpState
	x <- try $ getDirectoryContents y
	case x of
		Left er -> sendData (datahandle ftpState) (show (er::IOException))
		Right x -> return (show x) >>= sendData (datahandle ftpState) -- this would need to be changed to datahandle once we figure out how to get the datahandle. 


-- Need to add `finally` to close the opened file handle in handle_get
handle_rename :: String -> FTPState -> IO ()
handle_rename filename ftpState = do
	x <- doesFileExist (renamefile ftpState)
	case x of
		False -> sendData (cmdsocket ftpState) (show "File doesnt Exist")
		True -> do
				renameFile (renamefile ftpState) filename
				return ()---------------Check kar lena!!!
				sendData (cmdsocket ftpState) (show "250 rename successful")
			

-- Need to add `finally` to close the opened file handle in handle_get
handle_del :: String -> FTPState -> IO ()
handle_del filename ftpState = do
	x <- doesFileExist filename
	case x of
		False -> sendData (cmdsocket ftpState) (show "File doesnt Exist")
		True -> do
				removeFile filename
				return ()---------------Check kar lena!!!
			


-- Need to add `finally` to close the opened file handle in handle_get
handle_get :: String -> FTPState -> IO ()
handle_get filename ftpState = do
	x <- try $ openFile filename ReadMode
	case x of
		Left er -> sendData (datahandle ftpState) (show (er::IOException))
		Right x -> hGetContents x >>= sendData (datahandle ftpState) -- this would need to be changed to datahandle once we figure out how to get the datahandle. 
			
handle_put :: String -> FTPState -> IO ()
handle_put filename ftpState = do
	x <- try $ openFile filename WriteMode
	case x of
		Left er -> sendData (datahandle ftpState) (show (er::IOException))
		Right x -> hGetContents (datahandle ftpState) >>= sendData x -- this would need to be changed to datahandle once we figure out how to get the datahandle. 

worker :: Socket -> (Handle, HostName, PortNumber) ->  IO ()
worker sock2 (hand, host, port) = do
  tID <- myThreadId
  putStrLn $ show tID ++ " <- " ++ host ++ ":" ++ show port
  hSetBuffering hand LineBuffering
  sendData hand "220 Welcome to FTP Server."
  --proto <- getProtocolNumber "tcp"
  --sock <- socket AF_INET Stream proto
  let ftpState = initDefaultFTPState hand sock2 in
	ftpHandler ftpState `finally` do {hClose hand ; putStrLn $ "done " ++ host ++ ":" ++ show port}
