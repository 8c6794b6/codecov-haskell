module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List
import           Data.Maybe                 hiding (listToMaybe)
import           Network.URI
import           System.Console.CmdArgs
import           System.Environment         (getEnv, getEnvironment)
import           System.Exit                (exitFailure, exitSuccess)
import           Trace.Hpc.Codecov
import           Trace.Hpc.Codecov.Config   (Config (Config))
import qualified Trace.Hpc.Codecov.Config   as Config
import           Trace.Hpc.Codecov.Curl
import           Trace.Hpc.Codecov.Util

import           CodecovHaskellCmdLine

baseUrlApiV2 :: String
baseUrlApiV2 = "https://codecov.io/upload/v2"

class QueryParam q where
  qp_service   :: q -> String
  qp_service   = const "unknown"
  qp_branch    :: q -> IO String
  qp_branch    = const (return "")
  qp_build     :: q -> IO String
  qp_build     = const (return "")
  qp_build_url :: q -> IO String
  qp_build_url = const (return "")
  qp_commit    :: q -> IO String
  qp_commit    = const (return "")
  qp_flags     :: q -> IO String
  qp_flags     = const (return "")
  qp_job       :: q -> IO String
  qp_job       = const (return "")
  qp_name      :: q -> IO String
  qp_name      = const (return "")
  qp_slug      :: q -> IO String
  qp_slug      = const (return "")
  qp_env       :: q -> IO String
  qp_env       = const (return "")
  qp_tag       :: q -> IO String
  qp_tag       = const (return "")
  qp_pr        :: q -> IO String
  qp_pr        = const (return "")

data Travis = Travis

instance QueryParam Travis where
  qp_service _ = "travis"
  qp_branch _  = getEnv "TRAVIS_BRANCH"
  qp_build _   = getEnv "TRAVIS_JOB_NUMBER"
  qp_commit _  = getEnv "TRAVIS_COMMIT"
  qp_job _     = getEnv "TRAVIS_JOB_ID"
  qp_slug _    = getEnv "TRAVIS_REPO_SLUG"
  qp_env _     = getEnv "TRAVIS_OS_NAME"
  qp_tag _     = getEnv "TRAVIS_TAG"
  qp_pr _      = getEnv "TRAVIS_PULL_REQUEST"

data CircleCI = CircleCI

instance QueryParam CircleCI

data Jenkins = Jenkins

instance QueryParam Jenkins

composeParam :: QueryParam ci => ci -> IO String
composeParam ci =
  do let service = qp_service ci
         get_val acc (key,g,format) = do
           val <- g ci
           return ((key ++ '=':format val) : acc)
         kvs = [("branch", qp_branch, id)
               ,("build", qp_build, id)
               ,("build_url", qp_build_url,  id)
               ,("commit", qp_commit, id)
               ,("flags", qp_flags, id)
               ,("name", qp_name, urlencode)
               ,("job", qp_job, id)
               ,("slug", qp_slug, urlencode)
               ,("env", qp_env, id)
               ,("tag", qp_tag, id)
               ,("pr", qp_pr, drop_head_sharps)]
         urlencode = escapeURIString isUnescapedInURIComponent
         drop_head_sharps = dropWhile (== '#')

     params <- foldM get_val [("service" ++ '=':service)] kvs
     return $ concat (intersperse "&" params)

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
-- getUrlWithToken apiUrl _ Nothing = return apiUrl
-- getUrlWithToken apiUrl param (Just t) = return $ apiUrl ++ "&" ++ param ++ "=" ++ t
getUrlWithToken apiUrl param mb_val =
  apiUrl ++ '$':param ++ '=':fromMaybe "" mb_val

getConfig :: CodecovHaskellArgs -> Maybe Config
getConfig cha = do _testSuites <- listToMaybe (testSuites cha)
                   return Config { Config.excludedDirs = excludeDirs cha
                                 , Config.testSuites   = _testSuites
                                 , Config.tixDir       = tixFile cha
                                 , Config.mixDir       = mixDir cha
                                 , Config.srcDir       = srcDir cha
                                 }

main :: IO ()
main = do
    cha <- cmdArgs codecovHaskellArgs
    case getConfig cha of
        Nothing -> putStrLn "Please specify a target test suite name" >> exitSuccess
        Just config -> do
            codecovJson <- generateCodecovFromTix config
            when (displayReport cha) $ BSL.putStrLn $ encode codecovJson
            unless (dontSend cha) $ do
                apiUrl <- getUrlApiV2
                putStrLn ("API URL: " ++ apiUrl)
                let fullUrl = getUrlWithToken apiUrl "token" (token cha)
                response <- postJson (BSL.unpack $ encode codecovJson) fullUrl (printResponse cha)
                case response of
                    PostSuccess url _ -> do
                        let responseUrl = getUrlWithToken url "token" (token cha)
                        putStrLn ("URL: " ++ responseUrl)
                        -- wait 10 seconds until the page is available
                        threadDelay (10 * 1000000)
                        coverageResult <- readCoverageResult responseUrl (printResponse cha)
                        case coverageResult of
                            Just totalCoverage -> putStrLn ("Coverage: " ++ totalCoverage) >> exitSuccess
                            Nothing -> putStrLn "Failed to read total coverage" >> exitSuccess
                    PostFailure msg -> putStrLn ("Error: " ++ msg) >> exitFailure
