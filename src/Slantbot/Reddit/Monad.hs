{-# LANGUAGE RankNTypes #-}

module Slantbot.Reddit.Monad where

import           Algolia.Query
import           Control.Monad.Reader
import           Data.ByteString        (ByteString)
import           Reddit.Types.Reddit
import           Reddit.Types.User
import           Slantbot.Reddit.Config

type RBot a = RedditT (ReaderT RedditInfo IO) a

ask' :: RBot RedditInfo
ask' = lift ask

algoliaProfile' :: RBot AlgoliaProfile
algoliaProfile' = algoliaProfile <$> ask'

database' :: RBot ByteString
database' = database <$> ask'

maintainer' :: RBot Username
maintainer' = maintainer <$> ask'

username' :: RBot Username
username' = username <$> ask'
