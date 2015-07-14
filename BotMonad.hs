module BotMonad where

import AlgoliaQuery (AlgoliaProfile)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Reddit.Types.Comment (CommentID)
import Reddit.Types.Reddit (RedditT)

data BotInfo = BotInfo
             { _aprof :: !AlgoliaProfile
             , _dburl :: !ByteString
             , _owner :: !Text
             , _ruser :: !Text
             , _lastP :: !CommentID
             } deriving (Show)

type BotM a = RedditT (StateT CommentID (ReaderT BotInfo IO)) a

aprof' :: BotM AlgoliaProfile
aprof' = do
  BotInfo it _ _ _ _ <- lift $ lift ask
  return it

dburl' :: BotM ByteString
dburl' = do
  BotInfo _ it _ _ _ <- lift $ lift ask
  return it

owner' :: BotM Text
owner' = do
  BotInfo _ _ it _ _ <- lift $ lift ask
  return it

ruser' :: BotM Text
ruser' = do
  BotInfo _ _ _ it _ <- lift $ lift ask
  return it

lastP' :: BotM CommentID
lastP' = do
  BotInfo _ _ _ _ it <- lift $ lift ask
  return it

putLastP :: CommentID -> BotM ()
putLastP pid = do
  lastP <- lift get
  if (pid > lastP)
  then lift $ put pid
  else return ()
