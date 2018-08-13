{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_arvoreavl (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\chung\\OneDrive\\UFABC\\2018\\2\186 Quad 2018\\Parad. Prog\\C\243digos\\arvoreavl\\.stack-work\\install\\b6b92d36\\bin"
libdir     = "C:\\Users\\chung\\OneDrive\\UFABC\\2018\\2\186 Quad 2018\\Parad. Prog\\C\243digos\\arvoreavl\\.stack-work\\install\\b6b92d36\\lib\\x86_64-windows-ghc-8.4.3\\arvoreavl-0.1.0.0-K0P2O3A00964J8iFODjfS4-arvoreavl"
dynlibdir  = "C:\\Users\\chung\\OneDrive\\UFABC\\2018\\2\186 Quad 2018\\Parad. Prog\\C\243digos\\arvoreavl\\.stack-work\\install\\b6b92d36\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\chung\\OneDrive\\UFABC\\2018\\2\186 Quad 2018\\Parad. Prog\\C\243digos\\arvoreavl\\.stack-work\\install\\b6b92d36\\share\\x86_64-windows-ghc-8.4.3\\arvoreavl-0.1.0.0"
libexecdir = "C:\\Users\\chung\\OneDrive\\UFABC\\2018\\2\186 Quad 2018\\Parad. Prog\\C\243digos\\arvoreavl\\.stack-work\\install\\b6b92d36\\libexec\\x86_64-windows-ghc-8.4.3\\arvoreavl-0.1.0.0"
sysconfdir = "C:\\Users\\chung\\OneDrive\\UFABC\\2018\\2\186 Quad 2018\\Parad. Prog\\C\243digos\\arvoreavl\\.stack-work\\install\\b6b92d36\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arvoreavl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arvoreavl_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "arvoreavl_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "arvoreavl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arvoreavl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arvoreavl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
