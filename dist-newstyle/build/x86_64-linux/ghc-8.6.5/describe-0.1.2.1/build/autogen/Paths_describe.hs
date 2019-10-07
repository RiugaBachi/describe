{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_describe (
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
version = Version [0,1,2,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/riuga/.cabal/bin"
libdir     = "/home/riuga/.cabal/lib/x86_64-linux-ghc-8.6.5/describe-0.1.2.1-inplace"
dynlibdir  = "/home/riuga/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/riuga/.cabal/share/x86_64-linux-ghc-8.6.5/describe-0.1.2.1"
libexecdir = "/home/riuga/.cabal/libexec/x86_64-linux-ghc-8.6.5/describe-0.1.2.1"
sysconfdir = "/home/riuga/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "describe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "describe_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "describe_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "describe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "describe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "describe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
