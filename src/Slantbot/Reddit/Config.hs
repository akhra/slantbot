module Slantbot.Reddit.Config where

import           Algolia.Query
import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Reddit

-- TODO: Lens lens lens lens lens lens baked beans and lens.
data RedditConfig = RedditConfig
  { redditInfo      :: !RedditInfo
  , redditFrequency :: !Int
  , redditOptions   :: !RedditOptions
  }
instance Show RedditConfig where
  show x = "RedditConfig {redditInfo = " ++ show (redditInfo x)
        ++ ", redditFrequency = " ++ show (redditFrequency x)
        ++ ", redditOptions = <Reddit.RedditOptions>}"

data RedditInfo = RedditInfo
  { algoliaProfile :: !AlgoliaProfile
  , database       :: !ByteString
  , maintainer     :: !Text
  , username       :: !Text
  , lastParentID   :: !CommentID
  } deriving (Show)

updateLastParentID :: RedditConfig -> CommentID -> RedditConfig
updateLastParentID x c = x{redditInfo=(redditInfo x){lastParentID=c}}
