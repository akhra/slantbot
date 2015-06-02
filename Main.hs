{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AlgoliaParser
import AlgoliaQuery
import BotMonad
import SubredditDB
import RedditRevised
import Reddit.Types.Comment as RC
import Reddit.Types.Listing
--import Reddit.Types.Message as RM   -- messaging currently wonky
import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User
import Control.Concurrent (threadDelay)
import Control.Monad (liftM, when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple (connectPostgreSQL)
import System.Environment (getEnv)
import Data.Text (Text, pack, unpack, empty)
import qualified Data.ByteString.Char8 as B (pack)

main :: IO ()
main = do
  -- This is a hot mess of potential unhandled errors,
  -- but all of them happen immediately on execution.
  (info@(BotInfo _ _ _ ruser lastP), pass, freq) <- getConfig
  Right lastParent <- runReddit ruser pass $ do
    Listing _ _ cs <- getUserComments $ Username ruser
    case fmap (commentID) $ listToMaybe cs of
      Nothing     -> return Nothing
      Just myLast -> do
        c <- getCommentInfo myLast
        return $ inReplyTo c
  runBot pass freq $ info { _lastP = fromMaybe lastP lastParent }

getConfig :: IO (BotInfo, Text, Int)
getConfig = do
  let tEnv = liftM pack . getEnv
      bEnv = liftM B.pack . getEnv
  algid <- bEnv "ALGOLIA_ID"
  algky <- bEnv "ALGOLIA_KEY"
  algqb <- bEnv "ALGOLIA_QUERY_BASE"
  dburl <- bEnv "DATABASE_URL"
  owner <- tEnv "REDDIT_OWNER"
  rpass <- tEnv "REDDIT_PASS"
  ruser <- tEnv "REDDIT_USER"
  rfreq <- liftM read $ getEnv "REDDIT_FREQ"
  let aprof = AlgoliaProfile algid algky algqb
      info = BotInfo aprof dburl owner ruser (CommentID "0")
  return (info, rpass, rfreq * 1000000)

runBot :: Text -> Int -> BotInfo -> IO ()
runBot pass freq info@(BotInfo _ _ _ user lastPID) = do
  (_, lastPID') <- flip runReaderT info
                 $ flip runStateT lastPID
                 $ runRedditWithRateLimiting user pass bot
  when (freq > 0) $ threadDelay freq
  runBot pass freq $ info { _lastP = lastPID' }

bot :: BotM ()
bot = do
  -- Message handling seems NYI in the Reddit library.
  -- They can be fetched, but not sent or marked as read.
  -- Probably part of why it's not on Hackage yet...
--  Listing _ _ messages <- getUnread
--  mapM_ handleMessage messages
  dburl <- dburl'
  conn <- liftIO $ connectPostgreSQL dburl
  subs <- liftIO $ getSubs conn
  mapM_ scanSub $ fmap R subs

{-
handleMessage :: Message -> BotM ()
handleMessage m =  case messageID m of
  CommentMessage _ -> return ()
  PrivateMessage _ -> do
    owner <- owner'
    let Username sender = from m
    sendMessage (Username owner) mtopic (mbody sender)
  where
    mtopic  = "slantbot fwd: " <> (subject m)
    mbody s = "from /u/" <> s <> "\n\n---\n" <> (RM.body m)
-}

scanSub :: SubredditName -> BotM ()
scanSub sub = do
  Listing _ _ comments
    <- getNewComments' (Options Nothing (Just 100)) $ Just sub
  mapM_ handleComment comments

handleComment :: Comment -> BotM ()
handleComment comment = do
  lastP <- lastP'
  if lastP >= commentID comment
  then return ()
  else do
    self <- ruser'
    isSelf <- selfInvolved (Username self) comment
    if isSelf
    then return ()
    else case getQueryString (fmap toLower $ unpack self) comment of
        Nothing -> return ()
        Just q  -> respondToQuery q $ commentID comment

selfInvolved :: Username -> Comment -> BotM Bool
selfInvolved self comment = do
  if self == author comment
  then return True
  else case inReplyTo comment of
    Nothing       -> return False
    Just parentID -> do
      parent <- getCommentInfo parentID
      return $ self == author parent

getQueryString :: String -> Comment -> Maybe String
getQueryString self = check . lines . unpack . RC.body
  where
    check []    = Nothing
    check (x:_) = case words $ fmap toLower x of
      ('@':u):q | u == self -> Just $ unwords q
      _                     -> Nothing

respondToQuery :: String -> CommentID -> BotM ()
respondToQuery query cid = do
  aprof <- aprof'
  b <- liftIO $ getBest aprof query
  let response = (responseBody b) <> responseFooter
  _ <- reply cid response
  putLastP cid  -- This won't take effect until the next full bot cycle.
                -- Necessary because process order is by subreddit, not age.

responseBody :: Maybe Best -> Text
responseBody b = case b of
  Nothing   -> responseNoResult
  Just best -> responseBest best

responseNoResult :: Text
responseNoResult
  =  "Sorry, I couldn't find anything for that query."
     <> " I'll just sit in the corner and contemplate my failure."

responseBest :: Best -> Text
responseBest (Best qid qtitle oid otitle getURL)
  =  "Hi there! It looks like you're asking:  \n"
  <> "[*" <> qtitle <> "*](" <> qURL <> ")\n\n"
  <> "The most-recommended option is " <> option <> ".  \n"
  <> "If you want more info, you can:\n\n"
  <> "- [See its pros and cons]"
     <> "(" <> qURL <> "/viewpoints/" <> (pack $ show oid) <> ")\n\n"
  <> "- [Compare other options]" <> "(" <> qURL <> ")"
  where
    qURL = "http://www.slant.co/topics/" <> (pack $ show qid)
    option = if getURL == empty
             then "**" <> otitle <> "**"
             else "[**" <> otitle <> "**](" <> getURL <> ")"

responseFooter :: Text
responseFooter
  =  "\n\n---\n"
  <> "^(I am a bot! For safety, replies to this comment will be ignored."
    <> " Direct messages will be checked by a human.)"