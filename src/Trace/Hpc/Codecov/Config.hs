-- |
-- Module:      Trace.Hpc.Codecov.Config
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Module containing configuration data type.

module Trace.Hpc.Codecov.Config where

import Trace.Hpc.Tix

-- | Configuration to find tix, mix and source code files.
data Config = Config {
    excludedDirs :: ![FilePath],
    testSuites   :: ![String],
    tixDir       :: !FilePath,
    mixDir       :: !FilePath,
    srcDir       :: !FilePath
    }

defaultHpcDir :: FilePath
defaultHpcDir = "dist/hpc/"

defaultTixDir :: FilePath
defaultTixDir = defaultHpcDir ++ "tix/"

defaultMixDir :: FilePath
defaultMixDir = defaultHpcDir ++ "mix/"

getMixPaths :: Config -> String -> TixModule -> [FilePath]
getMixPaths config testSuiteName tix = do _dirName <- dirName
                                          return $ mixDir config ++ _dirName ++ "/"
    where dirName = case span (/= '/') modName of
              (_, [])        -> [ testSuiteName ]
              (packageId, _) -> [ "", packageId ]
          modName = tixModuleName tix

getTixPath :: Config -> String -> FilePath
getTixPath config testSuiteName =
  tixDir config ++ testSuiteName ++ "/" ++ getTixFileName testSuiteName
