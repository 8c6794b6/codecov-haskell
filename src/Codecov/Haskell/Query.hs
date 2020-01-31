-- |
-- Module:    Codecov.Haskell.Query
-- Copyright: (c) 2020 8c6794b6
-- License:   BSD3
-- Stability: experimental
--
-- Types and function for query parameters in URI.

module Codecov.Haskell.Query
  ( -- * Query parameter
    QueryParam(..)
  , composeParam

    -- * CI providers
  , Travis(..)
  , CircleCI(..)
  , Jenkins(..)
  ) where

-- base
import Control.Monad      (foldM)
import Data.List          (intersperse)
import System.Environment (getEnv)

-- filepath
import System.FilePath    (dropExtension, (</>))

-- network-uri
import Network.URI        (escapeURIString, isUnescapedInURIComponent)

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

-- | Data type to represent Travis CI.
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

-- | Data type to represent CircleCI.
data CircleCI = CircleCI

instance QueryParam CircleCI where
  qp_service  = const "circleci"
  qp_branch _ = getEnv "CIRCLE_BRANCH"
  qp_build _  = getEnv "CIRCLE_BUILD_NUM"
  qp_job _    = getEnv "CIRCLE_NODE_INDEX"
  qp_slug _   = do reponame <- getEnv "CIRCLE_PROJECT_REPONAME"
                   if null reponame
                     then do url0 <- getEnv "CIRCLE_REPOSITORY_URL"
                             let url1 = dropWhile (== ':') url0
                             return (dropExtension url1)
                     else do user <- getEnv "CIRCLE_PROJECT_USERNAME"
                             return (user </> reponame)
  qp_pr _     = getEnv "CIRCLE_PR_NUMBER"
  qp_commit _ = getEnv "CIRCLE_SHA1"

-- | Data type to represent Jenkins.
data Jenkins = Jenkins

instance QueryParam Jenkins where
  qp_service = const "jenkins"

-- | Compose URL parameters.
--
-- See below for detail:
--
--   - <https://docs.codecov.io/reference#upload Codecov documentation>
--   - <https://codecov.io/bash Codecov bash uploader>
--
composeParam :: QueryParam ci => ci -> IO String
composeParam ci =
  do let urlencode = escapeURIString isUnescapedInURIComponent
         drop_head_sharps = dropWhile (== '#')
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
         z = [("service" ++ '=':qp_service ci)]
     params <- foldM get_val z kvs
     return $ concat (intersperse "&" params)
