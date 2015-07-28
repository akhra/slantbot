{-# LANGUAGE OverloadedStrings #-}

module Algolia.Query (AlgoliaProfile(..), algoliaQuery) where

-- TODO: this all less bad

import           Algolia.Response      (AlgoliaResponse)
import           Control.Exception
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy  as L (ByteString)
import           Data.Default
import           Data.List
import           Data.Monoid
import           Network.HTTP.Conduit
import           Network.URI           (escapeURIString,
                                        isUnescapedInURIComponent)

-- TODO: Lens. Lens everywhere.
data AlgoliaProfile = AlgoliaProfile
  { appID      :: !ByteString
  , apiKey     :: !ByteString
  , baseQuery  :: !ByteString
  , queryIndex :: !ByteString
  , manager    :: !Manager
  }
instance Show AlgoliaProfile where
  show x =
    "AlgoliaProfile {appID = " ++ show (appID x)
    ++ ", apiKey = " ++ show (apiKey x)
    ++ ", baseQuery = " ++ show (baseQuery x)
    ++ ", queryIndex = " ++ show (queryIndex x)
    ++ ", manager = <Network.HTTP.Conduit.Manager>}"

algoliaQuery :: FromJSON a
             => (String -> String)
             -> ([String] -> [String])
             -> AlgoliaProfile
             -> String
             -> IO (Maybe (AlgoliaResponse a))
algoliaQuery groomChars groomWords profile query = do
  response <- runQuery (mkQuery groomChars groomWords profile query)
                       $ manager profile
  return $ case response of
    Right r -> go $ responseBody r
    _       -> Nothing
  where
    go r = case decode r of
      Nothing -> Nothing
      x       -> x

mkQuery :: (String -> String)
        -> ([String] -> [String])
        -> AlgoliaProfile
        -> String
        -> Request
mkQuery gC gW p q' =
  let q = ("&query=" <>) . pack . intercalate "+"
        . fmap (escapeURIString isUnescapedInURIComponent)
        . gW . words . gC $ q'
  in def { method         = "POST"
         , host           = appID p <> "-dsn.algolia.net"
         , path           = "/1/indexes/" <> (queryIndex p) <> "/query"
         , requestHeaders = [ ("X-Algolia-Application-Id", appID p)
                            , ("X-Algolia-API-Key", apiKey p) ]
         , requestBody    = RequestBodyBS $ "{ \"params\" : \""
                            <> (baseQuery p) <> q <> "\" }"
         }

runQuery :: Request
         -> Manager
         -> IO (Either HttpException (Response L.ByteString))
runQuery r m = try $ httpLbs r m
