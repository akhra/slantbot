module Slantbot.Reddit.Messages (handleMessages) where

import           Control.Monad
import           Data.Monoid
import           Reddit
import           Reddit.Types.Message
import           Slantbot.Reddit.Monad

handleMessages :: RBot ()
handleMessages = do
  captcha <- needsCaptcha
  unless captcha $ do
    Listing _ _ messages <- getUnread
    mapM_ handleMessage messages

handleMessage :: Message -> RBot ()
handleMessage message = do
  case from message of
    Just (Username sender) ->
      case messageID message of
        PrivateMessage _ -> do
          maintainer <- maintainer'
          sendMessage maintainer mtopic (mbody sender)
        _ -> return ()
  markRead message
  where
    mtopic = "fwd: " <> subject message
    mbody sender = "from /u/" <> sender <> "\n\n---\n" <> body message
