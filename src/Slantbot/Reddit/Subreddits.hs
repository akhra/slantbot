module Slantbot.Reddit.Subreddits (scanSubs) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Monoid
import           Data.Text                              (unpack)
import           Database.PostgreSQL.Simple
import           Reddit
import           Reddit.Types.Comment
import           Slantbot.Algolia
import           Slantbot.Reddit.Monad
import           Slantbot.Reddit.Subreddits.Permissions
import           Slantbot.Reddit.Subreddits.Responses

scanSubs :: RBot ()
scanSubs = do
  db <- database'
  conn <- liftIO $ connectPostgreSQL db
  subs <- liftIO $ getSubs conn
  liftIO $ close conn
  mapM_ scanSub $ fmap R subs

scanSub :: SubredditName -> RBot ()
scanSub sub = do
  Listing _ _ comments
    <- getNewComments' (Options Nothing (Just 100)) $ Just sub
  mapM_ handleComment comments

handleComment :: Comment -> RBot ()
handleComment comment = do
  lastParentID <- lastParentID'
  when (lastParentID < commentID comment) $ do
    self <- username'
    isSelf <- selfInvolved (Username self) comment
    unless isSelf $
      case getQueryString (toLower <$> unpack self) comment of
        Nothing -> return ()
        Just q  -> respondToQuery q comment

selfInvolved :: Username -> Comment -> RBot Bool
selfInvolved self comment = if self == author comment then return True else
  case inReplyTo comment of
    Nothing       -> return False
    Just parentID -> do
      parent <- getCommentInfo parentID
      return $ self == author parent

getQueryString :: String -> Comment -> Maybe String
getQueryString self = check . lines . unpack . body
  where
    check []    = Nothing
    check (x:_) = case words x of
      ('@':u):q | fmap toLower u == fmap toLower self
        ->  Just $ unwords q
      _ -> Nothing

respondToQuery :: String -> Comment -> RBot ()
respondToQuery q (Comment {commentID = cid, subreddit = (R sub) }) = do
  profile <- algoliaProfile'
  question <- liftIO $ questionSearch profile q
  let utm = "?utm_source=reddit&utm_medium=bot&utm_campaign=" <> sub
  _ <- reply cid $ response question utm
  putLastParentID cid
  -- ^^^ This won't take effect until the next full bot cycle.
  -- Necessary because process order is by subreddit, not age.
