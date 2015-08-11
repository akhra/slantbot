module Slantbot.Reddit.Subreddits (handleSubreddits) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Coerce
import           Data.Monoid
import           Data.Text                              (unpack)
import           Database.PostgreSQL.Simple
import           Reddit
import           Reddit.Types.Comment
import           Slantbot.Algolia
import           Slantbot.Reddit.Monad
import           Slantbot.Reddit.Subreddits.Permissions
import           Slantbot.Reddit.Subreddits.Responses

handleSubreddits :: RBot ()
handleSubreddits = mapM_ scanSub =<< getSubs' =<< database'
  where
    getSubs' db = liftIO $ do
      conn <- connectPostgreSQL db
      subs <- fmap R <$> getSubs conn
      close conn
      return subs

scanSub :: SubredditName -> RBot ()
scanSub sub = do
  self <- username'
  Listing _ _ comments
    <- getNewComments' (Options Nothing (Just 100)) $ Just sub
  mapM_ (handleComment self) comments

handleComment :: Username -> Comment -> RBot ()
handleComment self comment = do
  amParent <- checkParent
  unless (amAuthor || amParent)
    $ case getQueryString self comment of
    Just q -> do
      haveReplied <- checkChildren
      unless haveReplied $ respondToQuery q comment
    _ -> return ()
  where
    check c = author c == self
    amAuthor = check comment
    checkParent = case inReplyTo comment of
      Just parentID -> check <$> getCommentInfo parentID
      _ -> return False
    checkChildren = do
      children <- getMoreChildren (parentLink comment) [commentID comment]
      or <$> mapM resolveComment children
    resolveComment c' = case c' of
      Actual c -> return $ check c
      Reference _ cs -> any check <$> mapM getCommentInfo cs


getQueryString :: Username -> Comment -> Maybe String
getQueryString self = check . lines . unpack . body
  where
    check []    = Nothing
    check (x:_) = case words x of
      ('@':u):q | fmap toLower u == fmap toLower (unpack . coerce $ self)
        -> Just $ unwords q
      _ -> Nothing

respondToQuery :: String -> Comment -> RBot ()
respondToQuery q (Comment {commentID = cid, subreddit = (R sub)}) = do
  profile <- algoliaProfile'
  question <- liftIO $ questionSearch profile q
  let utm = "?utm_source=reddit&utm_medium=bot&utm_campaign=" <> sub
  reply cid $ response question utm
  return ()
