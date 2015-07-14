{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SubredditDB where

import Database.PostgreSQL.Simple (query, query_, execute, Only(..), Connection)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Text (Text)

data SP = Forbidden | Invoked | Automatic
  deriving (Show, Eq)

setSP :: Connection -> String -> SP -> IO ()  
setSP db sub' sp = do
  _ <- execute db
    "DELETE FROM subreddits WHERE subreddit=?"
    (Only sub)
  if (sp /= Forbidden)
     then put
     else return ()
  where
    sub = fmap toLower $ sub'
    put = do
      _ <- execute db
        "INSERT INTO subreddits VALUES (?,?)"
        (sub, (sp == Automatic))
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

getSubs :: Connection -> IO [Text]
getSubs db = do
  xs :: [Only (Maybe Text)]
    <- query_ db "SELECT (subreddit) FROM subreddits"
  return . catMaybes . fmap fromOnly $ xs
