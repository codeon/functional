import qualified Data.List.Split
import System.Process
import System.Directory
import Control.Concurrent
{-import qualified Control.Exception-} 
import Control.Exception 
import Control.Monad
import Network hiding (accept)
import System.IO
import Data.Text hiding (map, concat, last, head)
import Network.Socket
import Network.BSD (getProtocolNumber)
{-import System.IO.Error-}
import Network.Info
import Data.Word
import qualified Data.ByteString
import qualified Data.ByteString.Char8

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
                renamefile :: FilePath,
                hostip :: String, 
                isOpen :: Bool
                } 
                
initDefaultFTPState :: Handle  -> Socket -> String -> FTPState
initDefaultFTPState cmdHandle sock ip = FTPState
							  { logged_in = False,
								datatype = ASCII,
								connmode = NoChannel,
								cmdsocket = cmdHandle,
								datahandle = cmdHandle,
								datasocket = sock,
								curr_directory = getCurrentDirectory,
								renamefile = "",
								hostip = ip,
								isOpen = True
								}

listenPort :: PortID
listenPort = PortNumber 10024
dataPort :: PortID
dataPort = PortNumber 10023

main :: IO ()
main = do 
		cmdsock <- listenOn listenPort
		datasock <- listenOn dataPort
		acceptLoop cmdsock datasock `finally` sClose cmdsock

acceptLoop :: Socket -> Socket -> IO ()
acceptLoop cmdsock datasock = forever $ accept cmdsock >>= forkIO . (worker datasock)

printHere :: String -> IO ()
printHere mesg = do
	putStrLn mesg

sendData :: Handle -> String -> IO ()
sendData hand toBeSend = do
	hPutStrLn hand toBeSend

sendDataB :: Handle -> String -> IO ()
sendDataB hand toBeSend = do
	Data.ByteString.hPutStr hand $ Data.ByteString.Char8.pack toBeSend

rcvData :: Handle -> IO String
rcvData hand = do
	dataRecv <- (hGetLine hand)
	return dataRecv

ftpHandler :: FTPState -> IO()
ftpHandler ftpState = do
	x <- rcvData (cmdsocket ftpState) -- This line needs to be changed, should use cmdsocket from ftpState instead of directly using hand.
	y <- parse x ftpState
	z <- return (isOpen ftpState)
	if z then ftpHandler $ y else return ()


isdelm :: Char -> Bool
isdelm ' ' = True
isdelm  _ = False 

isdelmip :: Char -> Bool
isdelmip '.' = True
isdelmip ':' = True
isdelmip ',' = True
isdelmip _ = False 


parse :: String -> FTPState ->  IO FTPState
parse dataRecv ftpState = do
	printHere dataRecv
	let cmd:args = map unpack $ split isdelm $ strip $ pack dataRecv -- splitting the commands received on ' ' . The first word of recvData is the command and rest are arguments.
	case cmd of
		"RETR" -> do 
					
					--printHere $ show $ datahandle ftpState
					handle_get x ftpState 
					--sendData (datahandle ftpState) "150 send by data?"
					hClose (datahandle ftpState)
					--sClose (datasocket ftpState)
					
					return ftpState			
				where x:xs = args		
		"USER" -> do
					sendData (cmdsocket ftpState) "331 Password?"
					return ftpState
		"LIST" -> do 
					sendData (cmdsocket ftpState) "150 send by cmd?"
					--printHere $ show $ datahandle ftpState
					handle_ls ftpState 
					hClose (datahandle ftpState)
					sendData (cmdsocket ftpState) "226 Directory Send OK"
					return ftpState					
		"PORT" -> handle_PORT ftpState args
		"EPRT" -> handle_EPRT ftpState args
--				 ftpState
--		"EPRT" -> do {handle_PORT ftpState args}
--				  ftpState
		"PASS" -> do
					sendData (cmdsocket ftpState) "230 Login Successful !"
					return ftpState
		"STOR" -> do
					sendData (cmdsocket ftpState) "150 send me file"
					--printHere $ show $ datahandle ftpState
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
				    --sendData (cmdsocket ftpState) "250 File deleted"
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
					--printHere $ show $ renamefile ftpState
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
					sendData (cmdsocket ftpState) $ show $ cleanPath dir
					return ftpState {curr_directory = return $ cleanPath dir}
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
		"TYPE" -> do
					if(head args == "I")	then	sendData (cmdsocket ftpState) "200 Switching to Binary mode." else sendData (cmdsocket ftpState) "200 Switching to ASCII mode."			
					return ftpState
		"QUIT" -> do
					sendData (cmdsocket ftpState) "221 Goodbye."
					return ftpState {isOpen = False}
		"SYST" -> do
					sendData (cmdsocket ftpState) "215 UNIX Type: L8"
					--sendData (cmdsocket ftpState) "215 UNIX Type: L8"
					--sendData (cmdsocket ftpState) "Using binary mode to transfer files."
					--nUsing binary mode to transfer files.
					return ftpState
		_ -> do { sendData (cmdsocket ftpState) "Invalid Command Recieved" ; return ftpState}
--		_ -> retunr ftpState

handle_PASV :: FTPState -> IO FTPState
handle_PASV ftpState= do 
	--sock <- listenOn dataPort
	let [a,b,c,d,_] = map unpack $ split isdelmip $ strip $ pack $ hostip ftpState
	sendData (cmdsocket ftpState) ("227 Entering Passive Mode (" ++ a ++ "," ++ b ++ "," ++ c ++ "," ++ d ++ ",39,39).")
	(datasock, addr ) <- accept (datasocket ftpState)
	hand <- socketToHandle datasock ReadWriteMode
	--printHere $ show hand ++ ":" ++ show addr
	return $ ftpState {datahandle = hand }

porty :: String -> String -> Word16
porty x y = (read x) * 256 + (read y)

handle_PORT :: FTPState -> [String] -> IO FTPState
handle_PORT ftpState args = do
	--datasock <- socket AF_INET Stream defaultProtocol
	putStrLn $ "connecting to " ++ show ip ++ ":" ++ show port
	--addrs <- getAddrInfo (Just defaultHints) (Just ip) (Just http) 
	--let addr = head addrs sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr) connect sock (addrAddress addr)
	hand <- connectTo ip $ PortNumber port
	--hand <- socketToHandle datasock ReadWriteMode
	sendData (cmdsocket ftpState) "200 PORT command successful."
	return $ ftpState {datahandle = hand}   	
	where 
		x:xs = args
		[a,b,c,d,e,f] = map unpack $ split isdelmip $ strip $ pack x
		ip = a ++ "." ++ b ++ "." ++ c ++ "." ++ d
		port = PortNum ((read f) * 256 + (read e))

handle_EPRT :: FTPState -> [String] -> IO FTPState
handle_EPRT ftpState args = do
	--datasock <- socket AF_INET Stream defaultProtocol
	--putStrLn $ "connecting to " ++ show ip ++ ":" ++ show port ++ "e,f :" ++ show e ++ "," ++ show f
	--addrs <- getAddrInfo (Just defaultHints) (Just ip) (Just http) 
	--let addr = head addrs sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr) connect sock (addrAddress addr)
	hand <- connectTo ip $ PortNumber port
	--hand <- socketToHandle datasock ReadWriteMode
	sendData (cmdsocket ftpState) "200 PORT command successful."
	return $ ftpState {datahandle = hand}   	
	where 
		x:xs = args
		y = map unpack $ split isdelmip $ strip $ pack x
		ip = "127.0.0.1"
		port = PortNum (read $ last y)

cleanPath :: String -> String
cleanPath currentDirectory = 
		concat $ map alpha returnDir
		where returnDir = auxCleanPath (Data.List.Split.splitOn "/" currentDirectory) []

	
alpha :: String -> String
alpha "" = ""
alpha a = "/" ++ a


auxCleanPath :: [String] -> [String] -> [String]
auxCleanPath list returnValue = 
			case list of 
				(x:xs) -> case x of
								"." -> auxCleanPath xs returnValue
								".." -> auxCleanPath xs $ Prelude.init returnValue
								_ -> auxCleanPath xs $ returnValue ++ [x]
				[] -> returnValue

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
		False -> sendData (cmdsocket ftpState) "550 Directory Doesn't Exists!!!"
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
			True -> sendData (cmdsocket ftpState) "550 Directory Already Exists!!!"
			False -> do
				createDirectory $ newDir
				return ()---------------Check kar lena!!!
				sendData (cmdsocket ftpState) "250 Directory Created"


handle_ls :: FTPState -> IO ()
handle_ls ftpState = do
	path <- (curr_directory ftpState)
	(a, b, c, d) <- runInteractiveCommand $ "ls -l "++ (show path)
	hGetContents b >>= sendDataB (datahandle ftpState)


handle_rename :: String -> FTPState -> IO ()
handle_rename filename ftpState = do
	dir <- (curr_directory ftpState)
	fileN <- return (renamefile ftpState)
	oldRenFile <- return $ getPath dir fileN
   	newRenFile <- return $ getPath dir filename
	x <- doesFileExist oldRenFile
	case x of
		False -> sendData (cmdsocket ftpState) (show "550 File doesnt Exist")
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
		False -> sendData (cmdsocket ftpState) (show "550 File doesnt Exist")
		True -> do
				removeFile fileN
				sendData (cmdsocket ftpState) "250 File deleted"
				return ()---------------Check kar lena!!!
			


-- Need to add `finally` to close the opened file handle in handle_get
handle_get :: String -> FTPState -> IO ()
handle_get fileN ftpState = do
	dir <- (curr_directory ftpState)
	filename <- return $ getPath dir fileN
	x <- try $ openFile filename ReadMode
	case x of
		Left er -> sendDataB (cmdsocket ftpState) $ "550 " ++ (show (er::IOException))
		Right x -> do
					sendData (cmdsocket ftpState) "150 send by cmd?"
					hGetContents x >>= sendDataB (datahandle ftpState) -- this would need to be changed to datahandle once we figure out how to get the datahandle. 
					sendData (cmdsocket ftpState) "226 Transfer Comp"
			
handle_put :: String -> FTPState -> IO ()
handle_put fileN ftpState = do
	dir <- (curr_directory ftpState)
	filename <- return $ getPath dir fileN
	x <- try $ openFile filename WriteMode
	case x of
		Left er -> sendDataB (datahandle ftpState) (show (er::IOException))
		Right x -> hGetContents (datahandle ftpState) >>= sendDataB x -- this would need to be changed to datahandle once we figure out how to get the datahandle. 

worker :: Socket -> (Socket, SockAddr) ->  IO ()
worker datasock (cmdsock, clientaddr) = do
  hand <- socketToHandle cmdsock ReadWriteMode
  tID <- myThreadId
  hostaddr <- getSocketName cmdsock
  --addrinfo:xs <- getAddrInfo (Just defaultHints) (Just "mastimaan") (Just "ftp")
  --putStrLn $ show addrinfo
  putStrLn $ show tID ++ " :: client " ++ show clientaddr ++ " connected to server " ++ show hostaddr
  hSetBuffering hand LineBuffering
  sendData hand "220 Welcome to FTP Server."
  --proto <- getProtocolNumber "tcp"
  --sock <- socket AF_INET Stream proto
  let ftpState = initDefaultFTPState hand datasock (show hostaddr) in
	ftpHandler ftpState
	-- `finally` do {hClose hand ; putStrLn $ "done " ++ show clientaddr}
