{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:    Codecov.Haskell.CmdLine
-- Copyright: (c) 2020 8c6794b6
--            (c) 2014 Guillaume Nargeot
-- License:   BSD3
-- Stability: experimental
--
-- Command line argument handling for  @codecov-haskell@ executable.

module Codecov.Haskell.CmdLine where

-- base
import Data.List                (intercalate)
import Data.Version             (Version (..))

-- cmdargs
import System.Console.CmdArgs   (Data, Typeable, args, explicit, help,
                                 name, program, summary, typ, typDir,
                                 typFile, (&=))

-- Internal
import Paths_codecov_haskell    (version)
import Trace.Hpc.Codecov.Config (defaultMixDir, defaultTixDir)

data CodecovHaskellArgs = CmdMain
    { token         :: Maybe String
    , accessToken   :: Maybe String
    , mb_name       :: Maybe String
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
    , mb_name =
        Nothing
        &= explicit
        &= typ "TXT"
        &= name "name"
        &= help "Name label of coverage report"
    , excludeDirs   =
        []
        &= explicit
        &= typDir
        &= name "exclude-dir"
        &= help "Exclude sources files under the matching directory"
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
        &= help "Display the generated code coverage JSON"
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
