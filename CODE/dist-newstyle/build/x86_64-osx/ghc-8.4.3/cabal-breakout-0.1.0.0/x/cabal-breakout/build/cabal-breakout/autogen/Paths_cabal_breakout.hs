{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cabal_breakout (
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

bindir     = "/Users/duraniwalid/.cabal/bin"
libdir     = "/Users/duraniwalid/.cabal/lib/x86_64-osx-ghc-8.4.3/cabal-breakout-0.1.0.0-inplace-cabal-breakout"
dynlibdir  = "/Users/duraniwalid/.cabal/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/duraniwalid/.cabal/share/x86_64-osx-ghc-8.4.3/cabal-breakout-0.1.0.0"
libexecdir = "/Users/duraniwalid/.cabal/libexec/x86_64-osx-ghc-8.4.3/cabal-breakout-0.1.0.0"
sysconfdir = "/Users/duraniwalid/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cabal_breakout_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cabal_breakout_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cabal_breakout_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cabal_breakout_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cabal_breakout_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cabal_breakout_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
