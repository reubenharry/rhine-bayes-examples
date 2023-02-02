{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_vector (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,12,3,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/reubencohn-gordon/.cabal/store/ghc-9.2.5/vctr-0.12.3.1-51c8fefc/bin"
libdir     = "/Users/reubencohn-gordon/.cabal/store/ghc-9.2.5/vctr-0.12.3.1-51c8fefc/lib"
dynlibdir  = "/Users/reubencohn-gordon/.cabal/store/ghc-9.2.5/lib"
datadir    = "/Users/reubencohn-gordon/.cabal/store/ghc-9.2.5/vctr-0.12.3.1-51c8fefc/share"
libexecdir = "/Users/reubencohn-gordon/.cabal/store/ghc-9.2.5/vctr-0.12.3.1-51c8fefc/libexec"
sysconfdir = "/Users/reubencohn-gordon/.cabal/store/ghc-9.2.5/vctr-0.12.3.1-51c8fefc/etc"

getBinDir     = catchIO (getEnv "vector_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "vector_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "vector_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "vector_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vector_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vector_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
