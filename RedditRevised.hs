{-# LANGUAGE OverloadedStrings #-}

module RedditRevised
  ( module Export
  , runReddit
  , runRedditWithRateLimiting )
where
-- Changes an api-builder function to disable TLS certificate checking.

import Reddit.Actions as Export
import Reddit.Login
import Reddit.Types.Error as Export
import Reddit.Types.Reddit as Export hiding (info, should)

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Builder hiding (execAPI) -- changing this function
import Network.API.Builder.Error as Export (APIError)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit

execAPI ::
  MonadIO m => Builder -> s -> APIT s e m a -> m (Either (APIError e) a)
execAPI b s api = do
  m <- liftIO $ newManager
       $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
  (res, _, _) <- runAPI b m s api
  liftIO $ closeManager m
  return res

runReddit :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runReddit user pass = run user pass False

runRedditWithRateLimiting :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runRedditWithRateLimiting user pass = run user pass True

run :: MonadIO m => Text -> Text -> Bool -> RedditT m a -> m (Either (APIError RedditError) a)
run user pass shouldRateLimit (RedditT reddit) = do
  rli <- liftIO $ newTVarIO $ RateLimits shouldRateLimit Nothing
  execAPI builder rli $ do
    customizeRequest addHeader
    LoginDetails (Modhash mh) cj <- unRedditT $ login user pass
    customizeRequest $ \r ->
      addHeader r { cookieJar = Just cj
                  , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
    reddit
