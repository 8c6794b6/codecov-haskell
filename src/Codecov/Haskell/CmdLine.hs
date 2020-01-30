{-# LANGUAGE DeriveDataTypeable #-}

module Codecov.Haskell.CmdLine where

import Data.List
import Data.Version             (Version (..))
import Paths_codecov_haskell    (version)
import System.Console.CmdArgs
import Trace.Hpc.Codecov.Config (defaultMixDir, defaultTixDir)

data CodecovHaskellArgs = CmdMain
    { token         :: Maybe String
    , accessToken   :: Maybe String
    , excludeDirs   :: [String]
    , testSuites    :: [String]
    , tixFile       :: FilePath
    , mixDirs       :: [FilePath]
    , srcDirs       :: [FilePath]
    , displayReport :: Bool
    , printResponse :: Bool
    , dontSend      :: Bool
    } deriving (Data, Show, Typeable)

codecovHaskellArgs :: CodecovHaskellArgs
codecovHaskellArgs = CmdMain
    { token         =
        Nothing
        &= explicit
        &= typ "TXT"
        &= name "token"
        &= help "Codecov upload token for this repository"
    , accessToken   =
        Nothing
        &= explicit
        &= typ "TXT"
        &= name "access-token"
        &= help "Codecov access token to retrieve reports for private repos"
    , excludeDirs   =
        []
        &= explicit
        &= typDir
        &= name "exclude-dir"
        &= help "Exclude sources files under the matching directory from the coverage report"
    , tixFile        =
        defaultTixDir
        &= explicit
        &= typFile
        &= name "tix"
        &= help "Path to the .tix file"
    , mixDirs       =
        [defaultMixDir]
        &= explicit
        &= typDir
        &= name "mix-dir"
        &= help "Path to the directory containing \".mix\" files. \
                \Specify multiple times to pass more than one."
    , srcDirs =
        [""]
        &= explicit
        &= typDir
        &= name "src-dir"
        &= help "Relative directory from project root for source codes. \
                \Specify multiple times to pass more than one."
    , displayReport =
        False
        &= explicit
        &= name "display-report"
        &= help "Display the json code coverage report that will be sent to codecov.io"
    , printResponse =
        False
        &= explicit
        &= name "print-response"
        &= help "Prints the json reponse received from codecov.io"
    , dontSend      =
        False
        &= explicit
        &= name "dont-send"
        &= help "Do not send the report to codecov.io"
    , testSuites    =
        []
        &= typ "TEST-SUITE"
        &= args
    } &= summary ("codecov-haskell v" ++
                  versionString version ++
                  ", (C) Guillaume Nargeot 2014")
      &= program "codecov-haskell"
    where versionString = intercalate "." . map show . versionBranch
