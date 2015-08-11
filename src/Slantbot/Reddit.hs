module Slantbot.Reddit (runBot) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Reddit
import           Slantbot.Reddit.Config
import           Slantbot.Reddit.Messages
import           Slantbot.Reddit.Monad
import           Slantbot.Reddit.Subreddits

runBot :: RedditConfig -> IO ()
runBot config@(RedditConfig info freq opts) = do
  _ <- runReaderT (runRedditWith opts bot) info
  when (freq > 0) $ threadDelay freq
  runBot config

bot :: RBot ()
bot = handleMessages >> handleSubreddits
