module Slantbot.Reddit.Subreddits (handleSubreddits) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.Coerce
import           Data.Maybe
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
handleSubreddits = do
  self <- username'
  lastPID <- fromMaybe (CommentID "0") <$> getLastParentID self
  mapM_ (scanSub self lastPID) =<< getSubs' =<< database'
  where
    getSubs' db = liftIO $ do
      conn <- connectPostgreSQL db
      subs <- fmap R <$> getSubs conn
      close conn
      return subs

getLastParentID :: Username -> RBot (Maybe CommentID)
getLastParentID user = do
  comments <- getUserComments user
  case comments of
    (Listing _ _ cs)
      -> findM (fmap inReplyTo . getCommentInfo . commentID) cs
    _ -> return Nothing
  where
    findM f = runMaybeT . msum . map (MaybeT . f)

scanSub :: Username -> CommentID -> SubredditName -> RBot ()
scanSub self lastPID sub = do
  Listing _ _ comments
    <- getNewComments' (Options Nothing (Just 100)) $ Just sub
  mapM_ (handleComment self lastPID) comments

handleComment :: Username -> CommentID -> Comment -> RBot ()
handleComment self lastPID comment = do
  amParent <- checkParent
  unless (oldComment || amAuthor || amParent)
    $ case getQueryString self comment of
    Just q -> do
      haveReplied <- checkChildren
      unless haveReplied $ respondToQuery q comment
    _ -> return ()
  where
    oldComment = lastPID >= commentID comment
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
