-- |
-- Module:      Trace.Hpc.Codecov.Config
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Module containing configuration data type.

module Trace.Hpc.Codecov.Config where

-- | Configuration to find tix, mix and source code files.
data Config = Config {
    excludedDirs :: ![FilePath],
    testSuites   :: ![String],
    tixDir       :: !FilePath,
    mixDir       :: !FilePath,
    srcDir       :: !FilePath
    }
