{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_SPL (
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

bindir     = "/Users/ramy/projects/ProductLineAnalysis/haskell/SPL/.stack-work-profile/install/x86_64-osx/lts-8.0/8.0.2/bin"
libdir     = "/Users/ramy/projects/ProductLineAnalysis/haskell/SPL/.stack-work-profile/install/x86_64-osx/lts-8.0/8.0.2/lib/x86_64-osx-ghc-8.0.2/SPL-0.1.0.0-1gMIn6ueMVA5fHj2IG3fJG"
dynlibdir  = "/Users/ramy/projects/ProductLineAnalysis/haskell/SPL/.stack-work-profile/install/x86_64-osx/lts-8.0/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/ramy/projects/ProductLineAnalysis/haskell/SPL/.stack-work-profile/install/x86_64-osx/lts-8.0/8.0.2/share/x86_64-osx-ghc-8.0.2/SPL-0.1.0.0"
libexecdir = "/Users/ramy/projects/ProductLineAnalysis/haskell/SPL/.stack-work-profile/install/x86_64-osx/lts-8.0/8.0.2/libexec"
sysconfdir = "/Users/ramy/projects/ProductLineAnalysis/haskell/SPL/.stack-work-profile/install/x86_64-osx/lts-8.0/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SPL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SPL_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SPL_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SPL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SPL_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SPL_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
