module Paths_ftpserver (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/kritika/bin"
libdir     = "/Users/kritika/lib"
datadir    = "/Users/kritika/share"
libexecdir = "/Users/kritika/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ftpserver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ftpserver_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ftpserver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ftpserver_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
