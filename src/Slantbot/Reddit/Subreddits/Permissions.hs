{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slantbot.Reddit.Subreddits.Permissions (getSubs) where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple hiding (Automatic)

data SP = Forbidden | Invoked | Automatic
  deriving (Show, Eq)

getSubs :: Connection -> IO [Text]
getSubs db = do
  xs :: [Only (Maybe Text)]
    <- query_ db "SELECT (subreddit) FROM subreddits"
  return . catMaybes . fmap fromOnly $ xs

-- These are currently for GHCI use. But, maybe one day...
setSP :: Connection -> String -> SP -> IO ()
setSP db sub' sp = do
  _ <- execute db
    "DELETE FROM subreddits WHERE subreddit=?"
    (Only sub)
  when (sp /= Forbidden) put
  where
    sub = toLower <$> sub'
    put = do
      _ <- execute db
        "INSERT INTO subreddits VALUES (?,?)"
        (sub, sp == Automatic)
      return ()

getSP :: Connection -> String -> IO SP
getSP db sub' = do
  xs :: [(Text, Maybe Bool)] <- query db
    "select * from subreddits where subreddit=?"
    (Only sub)
  return $ go xs
  where sub          = fmap toLower sub'
        go []        = Forbidden
        go ((_,p):_) | p == Just True = Automatic
                     | otherwise      = Invoked
