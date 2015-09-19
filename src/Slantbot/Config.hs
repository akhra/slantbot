module Slantbot.Config where

import           Algolia.Query
import           Control.Monad
import qualified Data.ByteString.Char8  as B
import           Data.Default
import qualified Data.Text              as T
import           Network.Connection
import           Network.HTTP.Conduit
import           Reddit
import           Slantbot.Reddit.Config
import           System.Environment

data Config = Config
  { reddit :: !RedditConfig
--  , twitter :: !TwitterConfig
  } deriving (Show)

getConfig :: IO Config
getConfig = do
  let
    tEnv = liftM T.pack . getEnv
    bEnv = liftM B.pack . getEnv
  algid <- bEnv "ALGOLIA_ID"
  algky <- bEnv "ALGOLIA_KEY"
  algqb <- bEnv "ALGOLIA_QUERY_BASE"
  algix <- bEnv "ALGOLIA_QUERY_INDEX"
  dburl <- bEnv "DATABASE_URL"
  owner <- tEnv "REDDIT_OWNER"
  ruser <- tEnv "REDDIT_USER"
  rpass <- tEnv "REDDIT_PASS"
  rfreq <- (1000000*).read <$> getEnv "REDDIT_FREQ"
  httpM <- newManager
    $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
  let
    aprof = AlgoliaProfile algid algky algqb algix httpM
    rinfo = RedditInfo aprof dburl (Username owner) (Username ruser)
    ropts = def { connectionManager = Just httpM
                , loginMethod       = Credentials ruser rpass }
    rconf = RedditConfig rinfo rfreq ropts
  return $ Config rconf --tconf
