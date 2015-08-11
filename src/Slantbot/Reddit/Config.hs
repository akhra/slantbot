module Slantbot.Reddit.Config where

import           Algolia.Query
import           Data.ByteString (ByteString)
import           Reddit

data RedditConfig = RedditConfig
  { redditInfo      :: !RedditInfo
  , redditFrequency :: !Int
  , redditOptions   :: !RedditOptions
  }
instance Show RedditConfig where
  show x = "RedditConfig {redditBaseInfo = " ++ show (redditInfo x)
        ++ ", redditFrequency = " ++ show (redditFrequency x)
        ++ ", redditOptions = <RedditOptions>}"

data RedditInfo = RedditInfo
  { algoliaProfile :: !AlgoliaProfile
  , database       :: !ByteString
  , maintainer     :: !Username
  , username       :: !Username
  } deriving (Show)
