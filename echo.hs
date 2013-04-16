import Data.List.Split
import System.Process
import System.Directory
import Control.Concurrent
{-import qualified Control.Exception-} 
import Control.Exception 
import Control.Monad
import Network
import System.IO
import Data.Text hiding (map, concat)
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
	let cmd:args = map unpack $ Data.Text.split isdelm $ strip $ pack dataRecv -- splitting the commands received on ' ' . The first word of recvData is the command and rest are arguments.
	case cmd of
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
		"MKD" -> do
					handle_mkdir x ftpState
					return ftpState
				where x:xs = args
		"RMD" -> do
					handle_rmdir x ftpState
					return ftpState
				where x:xs = args
		"PWD" -> do
					dir <- (curr_directory ftpState)
					sendData (cmdsocket ftpState) $ show dir
					return ftpState --{curr_directory = return $ cleanPath dir}
		"CWD" -> do
					ftpState <- handle_cd x ftpState
					newDir <- (curr_directory ftpState)
					printHere newDir
					return ftpState
				where x:xs = args
		"CDUP" -> do
					ftpState <- handle_cd "../" ftpState
					newDir <- (curr_directory ftpState)
					printHere newDir
					return ftpState				
					
		"PASV" -> handle_PASV ftpState
		"EPSV" -> handle_PASV ftpState
		"SYST" -> do
					printHere dataRecv
					sendData (cmdsocket ftpState) "215 UNIX Type: L8"
					--sendData (cmdsocket ftpState) "Kuch"
					return ftpState
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


--cleanPath :: String -> String
--cleanPath currentDirectory = 
--		list = splitOn "/" currentDirectory

--auxCleanPath list returnValue		
					

getPath :: String -> String -> String
getPath currentDirectory filePath = do
					case x of
						'/' ->	filePath
						_ ->  currentDirectory ++ "/" ++ filePath
			where 
				x:xs = filePath

handle_cd :: String -> FTPState -> IO FTPState
handle_cd directoryName ftpState = do
					y <- curr_directory ftpState
					case x of
						'/' -> do 
								return ()
								isExist <- doesDirectoryExist newDir
								case isExist of
									False -> do
												sendData (cmdsocket ftpState) "550 Directory Doesn't Exists!!!"
												return ftpState
									True -> do
												--setCurrentDirectory newDir
												--return $ ftpState {curr_directory = return newDir}
												printHere newDir
												return ()---------------Check kar lena!!!
												sendData (cmdsocket ftpState) "250 Directory Changed"
												return ftpState {curr_directory = return newDir}
							where 
								newDir = if (Prelude.last directoryName == '/') then (Prelude.init directoryName) else directoryName
						_ -> do 
								return()
								isExist <- doesDirectoryExist newDir
								case isExist of
									False -> do
												sendData (cmdsocket ftpState) "550 Directory Doesn't Exists!!!"
												return ftpState
									True -> do
												--setCurrentDirectory newDir
												--return $ ftpState {curr_directory = return newDir}
												printHere newDir
												return ()---------------Check kar lena!!!
												sendData (cmdsocket ftpState) "250 Directory Changed"
												return ftpState {curr_directory = return newDir}
							where 
								newDir = if (Prelude.last directoryName == '/') then y ++ "/" ++ (Prelude.init directoryName) else y ++ "/" ++ directoryName
			where 
				x:xs = directoryName


handle_rmdir :: String -> FTPState -> IO ()
handle_rmdir directoryName ftpState = do
	z <- (curr_directory ftpState)
	newDir <- return $ getPath z directoryName
	x <- doesDirectoryExist $ newDir
	case x of
		False -> sendData (cmdsocket ftpState) "Directory Doesn't Exists!!!"
		True -> do
				y <- try $ removeDirectory $ newDir
				case y of
					Right y -> do
								return ()---------------Check kar lena!!!
								sendData (cmdsocket ftpState) "250 Directory Removed"
					Left er -> sendData (cmdsocket ftpState) (show (er::IOException))


handle_mkdir :: String -> FTPState -> IO ()
handle_mkdir directoryName ftpState = do
		z <- (curr_directory ftpState)
		newDir <- return $ getPath z directoryName
		x <- doesDirectoryExist $ newDir
		case x of
			True -> sendData (cmdsocket ftpState) "Directory Exists!!!"
			False -> do
				createDirectory $ newDir
				return ()---------------Check kar lena!!!
				sendData (cmdsocket ftpState) "250 Directory Created"


handle_ls :: FTPState -> IO ()
handle_ls ftpState = do
	path <- (curr_directory ftpState)
	(a, b, c, d) <- runInteractiveCommand $ "ls -l "++ (show path)
	hGetContents b >>= sendData (datahandle ftpState)


handle_rename :: String -> FTPState -> IO ()
handle_rename filename ftpState = do
	dir <- (curr_directory ftpState)
	fileN <- return (renamefile ftpState)
	oldRenFile <- return $ getPath dir fileN
   	newRenFile <- return $ getPath dir filename
	x <- doesFileExist oldRenFile
	case x of
		False -> sendData (cmdsocket ftpState) (show "File doesnt Exist")
		True -> do
				renameFile oldRenFile newRenFile
				return ()---------------Check kar lena!!!
				sendData (cmdsocket ftpState) (show "250 rename successful")
			

handle_del :: String -> FTPState -> IO ()
handle_del filename ftpState = do
	dir <- (curr_directory ftpState)
	fileN <- return $ getPath dir filename
	x <- doesFileExist fileN
	case x of
		False -> sendData (cmdsocket ftpState) (show "File doesnt Exist")
		True -> do
				removeFile fileN
				return ()---------------Check kar lena!!!
			


-- Need to add `finally` to close the opened file handle in handle_get
handle_get :: String -> FTPState -> IO ()
handle_get fileN ftpState = do
	dir <- (curr_directory ftpState)
	filename <- return $ getPath dir fileN
	x <- try $ openFile filename ReadMode
	case x of
		Left er -> sendData (datahandle ftpState) (show (er::IOException))
		Right x -> hGetContents x >>= sendData (datahandle ftpState) -- this would need to be changed to datahandle once we figure out how to get the datahandle. 
			
handle_put :: String -> FTPState -> IO ()
handle_put fileN ftpState = do
	dir <- (curr_directory ftpState)
	filename <- return $ getPath dir fileN
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
