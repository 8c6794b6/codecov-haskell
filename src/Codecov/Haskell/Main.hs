module Codecov.Haskell.Main (defaultMain) where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List
import           Data.Maybe                 hiding (listToMaybe)
import           Network.Curl               (URLString)
import           System.Console.CmdArgs
import           System.Environment         (getEnvironment)
import           System.Exit                (exitFailure)
import           Trace.Hpc.Codecov
import           Trace.Hpc.Codecov.Config   (Config (Config))
import qualified Trace.Hpc.Codecov.Config   as Config
import           Trace.Hpc.Codecov.Curl
import           Trace.Hpc.Codecov.Util

import           Codecov.Haskell.CmdLine
import           Codecov.Haskell.Query

baseUrlApiV2 :: String
baseUrlApiV2 = "https://codecov.io/upload/v2"

getUrlApiV2 :: IO String
getUrlApiV2 =
  do env <- getEnvironment
     let make_params
           | has "TRAVIS"       = composeParam Travis
           | has "CIRCLECI"     = composeParam CircleCI
           | has "JENKINS_HOME" = composeParam Jenkins
           | otherwise          = error "Unsupported CI service."
           where has key = isJust (lookup key env)
     params <- make_params
     return $ baseUrlApiV2 ++ '?':params

-- getUrlApiV2 :: IO String
-- getUrlApiV2 = do
--     env <- getEnvironment
--     case snd <$> find (isJust . flip lookup env . fst) ciEnvVars of
--         Just (service, idParamEnvVar, commitEnvVar, branchEnvVar) -> do
--             idParamValue <- getEnv idParamEnvVar
--             commit <- getEnv commitEnvVar
--             branch <- getEnv branchEnvVar
--             return $ baseUrlApiV2 ++
--                      "?job=" ++ idParamValue ++
--                      "&commit=" ++ commit ++
--                      "&branch=" ++ branch ++
--                      "&service=" ++ service
--         _ -> error "Unsupported CI service."
--     where ciEnvVars = [
--            ("TRAVIS", ("travis", "TRAVIS_JOB_ID", "TRAVIS_COMMIT", "TRAVIS_BRANCH")),
--            ("JENKINS_HOME", ("jenkins", "BUILD_NUMBER", "GIT_COMMIT", "GIT_BRANCH")),
--            ("CIRCLECI", ("circleci", "CIRCLE_BUILD_NUM", "CIRCLE_SHA1", "CIRCLE_BRANCH"))]

getUrlWithToken :: String -> String -> Maybe String -> String
getUrlWithToken apiUrl key =
  maybe apiUrl (((apiUrl ++ '&':key) ++) . ('=':))

getConfig :: CodecovHaskellArgs -> Maybe Config
getConfig cha =
  do _testSuites <- listToMaybe (testSuites cha)
     return Config { Config.excludedDirs = excludeDirs cha
                   , Config.testSuites   = _testSuites
                   , Config.tixDir       = tixFile cha
                   , Config.mixDirs      = mixDirs cha
                   , Config.srcDirs      = srcDirs cha
                   }

printCoverage :: CodecovHaskellArgs -> URLString -> IO ()
printCoverage cha url =
  do let responseUrl = getUrlWithToken url "token" (token cha)
     putStrLn ("URL: " ++ responseUrl)
     -- wait 10 seconds until the page is available
     threadDelay (10 * 1000000)
     coverageResult <- readCoverageResult responseUrl (printResponse cha)
     case coverageResult of
         Just totalCoverage -> putStrLn ("Coverage: " ++ totalCoverage)
         Nothing            -> putStrLn "Failed to read total coverage"

defaultMain :: IO ()
defaultMain = do
    cha <- cmdArgs codecovHaskellArgs
    case getConfig cha of
        Nothing -> putStrLn "Please specify a target test suite name"
        Just config -> do
            codecovJson <- generateCodecovFromTix config
            when (displayReport cha) $
              BSL.putStrLn $ encode codecovJson
            unless (dontSend cha) $ do
                apiUrl0 <- getUrlApiV2
                let apiUrl1 = getUrlWithToken apiUrl0 "name" (mb_name cha)
                    fullUrl = getUrlWithToken apiUrl1 "token" (token cha)
                response <- postJson (BSL.unpack $ encode codecovJson)
                                     fullUrl (printResponse cha)
                case response of
                    PostSuccess _url _ ->
                      -- XXX: Printing coverage response disabled.
                      -- printCoverage cha url
                      putStrLn "Successfully posted coverage report"
                    PostFailure msg ->
                      putStrLn ("Error: " ++ msg) >> exitFailure
