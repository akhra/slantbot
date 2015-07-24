{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AlgoliaParser
import AlgoliaQuery
import SubredditDB
import BotMonad
import Reddit
import Reddit.Types.Comment (Comment(..), commentID, inReplyTo, author)
import qualified Reddit.Types.Comment as RC
import Reddit.Types.Message (messageID, from, subject)
import qualified Reddit.Types.Message as RM
import Control.Concurrent (threadDelay)
import Control.Monad (liftM, when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit (newManager, mkManagerSettings)
import System.Environment (getEnv)
import Data.Text (Text, pack, unpack, empty)
import qualified Data.ByteString.Char8 as B (pack)

main :: IO ()
main = do
  -- This is a hot mess of potential unhandled errors,
  -- but all of them happen immediately on execution.
  (info@(BotInfo _ _ _ user lastParent), pass, freq) <- getConfig
  m <- newManager
    $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
  let options = RedditOptions True (Just m) (Credentials user pass) Nothing
  Right lastParent' <- runRedditWith options{loginMethod = Anonymous} $ do
    Listing _ _ cs <- getUserComments $ Username user
    case fmap (commentID) $ listToMaybe cs of
      Nothing     -> return Nothing
      Just myLast -> do
        c <- getCommentInfo myLast
        return $ inReplyTo c
  runBot freq options info{_lastP = fromMaybe lastParent lastParent'}

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
      info  = BotInfo aprof dburl owner ruser (CommentID "0")
  return (info, rpass, rfreq * 1000000)

runBot :: Int -> RedditOptions -> BotInfo -> IO ()
runBot freq options info@(BotInfo _ _ _ _ lastParent) = do
  (_, lastParent') <- flip runReaderT info . flip runStateT lastParent
                      $ runRedditWith options bot
  when (freq > 0) $ threadDelay freq
  runBot freq options info{_lastP = lastParent'}

bot :: BotM ()
bot = do
  Listing _ _ messages <- getUnread
  mapM_ handleMessage messages
  dburl <- dburl'
  conn <- liftIO $ connectPostgreSQL dburl
  subs <- liftIO $ getSubs conn
  mapM_ scanSub $ fmap R subs

handleMessage :: Message -> BotM ()
handleMessage message = do
  case from message of
    Just (Username sender) ->
      case messageID message of
        PrivateMessage _ -> do
          owner <- owner'
          sendMessage (Username owner) mtopic (mbody sender)
  markRead message
  where
    mtopic = "fwd: " <> (subject message)
    mbody sender = "from /u/" <> sender <> "\n\n---\n" <> (RM.body message)

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
        Just q  -> respondToQuery q comment

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
    check (x:_) = case words x of
      ('@':u):q | fmap toLower u == fmap toLower self 
        -> Just $ unwords q
      _ -> Nothing

respondToQuery :: String -> Comment -> BotM ()
respondToQuery query comment@(Comment{commentID=cid}) = do
  aprof <- aprof'
  b <- liftIO $ getBest aprof query
  let response = (responseBody comment b) <> responseFooter
  _ <- reply cid response
  putLastP cid  -- This won't take effect until the next full bot cycle.
                -- Necessary because process order is by subreddit, not age.

responseBody :: Comment -> Maybe Best -> Text
responseBody c b = case b of
  Nothing   -> responseNoResult
  Just best -> responseBest c best

responseNoResult :: Text
responseNoResult
  =  "Sorry, I couldn't find anything for that query."
     <> " I'll just sit in the corner and contemplate my failure."

responseBest :: Comment -> Best -> Text
responseBest (Comment{subreddit=(R sub)}) (Best qid qtitle oid otitle getURL)
  =  "Hi there! It looks like you're asking:  \n"
  <> "[*" <> qtitle <> "*](" <> qURL <> ")\n\n"
  <> "The most-recommended option is " <> option <> ".  \n"
  <> "If you want more info, you can:\n\n"
  <> "- [See its pros and cons]" <> "(" <> oURL <> ")\n\n"
  <> "- [See the other options]" <> "(" <> qURL <> ")"
  where
    url = "http://www.slant.co/topics/" <> (pack $ show qid)
    utm = "?utm_source=reddit&utm_medium=bot&utm_campaign=" <> sub
    qURL = url <> utm
    oURL = url <> "/viewpoints/" <> (pack $ show oid) <> utm
    option = if getURL == empty
             then "**" <> otitle <> "**"
             else "[**" <> otitle <> "**](" <> getURL <> ")"

responseFooter :: Text
responseFooter
  =  "\n\n---\n"
  <> "^(I am a bot! For safety, replies to this comment will be ignored."
    <> " Direct messages will be checked by a human.)"