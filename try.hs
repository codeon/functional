import System.Directory
import System.Posix.Files
import System.Process
import Control.Concurrent
{-import qualified Control.Exception-} 
import Control.Exception 
import Control.Monad
import Network
import System.IO
import Data.Text hiding (map)
import Network.Socket hiding (accept)
import Network.BSD (getProtocolNumber)

getls ftpState = do
	(a, b, c, d) <- runInteractiveCommand concat ["ls -l ", (curr_directory ftpState)]
	x<- hGetContents b
	putStrLn $ x
	
x= getCurrentDirectory
main = do 
		x <- getCurrentDirectory
		y <- getDirectoryContents x
		getDirectoryDetails y

getDirectoryDetails  :: [FilePath] -> IO ()
getDirectoryDetails [] = return ()
getDirectoryDetails (file:x) = do
								n <- getFileStatus file
								putStrLn (show $ fileMode n)
								getDirectoryDetails x
		
