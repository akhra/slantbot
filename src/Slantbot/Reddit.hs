{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slantbot.Reddit (launch) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Reddit
import           Reddit.Types.Comment
import           Slantbot.Reddit.Config
import           Slantbot.Reddit.Messages
import           Slantbot.Reddit.Subreddits

launch :: RedditConfig -> IO ()
launch config = do
  lastPID <- checkLastPID config
  runBot $ updateLastParentID config lastPID

checkLastPID :: RedditConfig -> IO CommentID
checkLastPID (RedditConfig (RedditInfo _ _ _ user lastPID) _ opts) =
  runRedditWith opts{loginMethod = Anonymous} go >>= \case
    Right (Just lastPID') -> return lastPID'
    _                     -> return lastPID
  where
    -- Why did I have to find this on Stack? Oughtta be in the Prelude!
    findM f = runMaybeT . msum . map (MaybeT . f)
    go = getUserComments (Username user) >>= \case
      (Listing _ _ cs) -> findM (fmap inReplyTo . getCommentInfo . commentID) cs

runBot :: RedditConfig -> IO ()
runBot config@(RedditConfig info freq opts) = do
  (_, lastPID) <- flip runReaderT info
                . flip runStateT (lastParentID info)
                . runRedditWith opts $ bot
  when (freq > 0) $ threadDelay freq
  runBot $ updateLastParentID config lastPID
  where
    bot = do
      captcha <- needsCaptcha
      unless captcha scanMessages
      scanSubs
