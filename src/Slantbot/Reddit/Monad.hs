{-# LANGUAGE RankNTypes #-}

module Slantbot.Reddit.Monad where

import           Algolia.Query
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import           Reddit.Types.Comment
import           Reddit.Types.Reddit
import           Slantbot.Reddit.Config

type RBot a = RedditT (StateT CommentID (ReaderT RedditInfo IO)) a

ask' :: RBot RedditInfo
ask' = lift $ lift ask

algoliaProfile' :: RBot AlgoliaProfile
algoliaProfile' = algoliaProfile <$> ask'

database' :: RBot ByteString
database' = database <$> ask'

username' :: RBot Text
username' = username <$> ask'

maintainer' :: RBot Text
maintainer' = maintainer <$> ask'

lastParentID' :: RBot CommentID
lastParentID' = lastParentID <$> ask'

putLastParentID :: CommentID -> RBot ()
putLastParentID pid = do
  pid' <- lift get
  when (pid > pid') (lift $ put pid)
