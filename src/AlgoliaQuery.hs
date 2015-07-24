{-# LANGUAGE OverloadedStrings #-}

module AlgoliaQuery where

import AlgoliaParser
import Control.Exception (try)
import Data.Aeson (decode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Char (toLower)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.List.Split (splitOneOf)
import Network.HTTP.Conduit ( Request(..), RequestBody(RequestBodyBS)
                            , Response(responseBody), HttpException
                            , withManager, httpLbs )
import Network.URI (escapeURIString, isUnescapedInURIComponent)
import qualified Data.ByteString.Lazy as L (ByteString)

data AlgoliaProfile = AlgoliaProfile
                      { appID     :: !ByteString
                      , apiKey    :: !ByteString
                      , baseQuery :: !ByteString
                      } deriving (Eq, Show)

-- NOTE: query string is assumed to be already toLower'd
algoliaQuery :: AlgoliaProfile -> String -> Request
algoliaQuery profile query =
  let q = ("&query=" <>) . pack . intercalate "+"
          . fmap (escapeURIString isUnescapedInURIComponent)
          . filter strip . splitOneOf " ?" . fmap toLower
          $ query
  in def { method         = "POST"
         , host           = appID profile <> "-dsn.algolia.net"
         , path           = "/1/indexes/questions/query"
         , requestHeaders =
           [ ("X-Algolia-Application-Id", appID profile)
           , ("X-Algolia-API-Key", apiKey profile) ]
         , requestBody    = RequestBodyBS $
           ("{ \"params\" : \"" <>) . (baseQuery profile <>) $ q <> "\" }"
         }
  where
    strip = not . flip elem [ "what", "is", "the", "best", "are", "for"
                            , "to", "of", "with", "a", "an", "on", "in"
                            , "" ]

runQuery :: Request -> IO (Either HttpException (Response L.ByteString))
runQuery req = try $ withManager $ httpLbs req

getBest :: AlgoliaProfile -> String -> IO (Maybe Best)
getBest _    [] = return Nothing
getBest prof q  = do
  response <- runQuery $ algoliaQuery prof q
  return $ case response of
    Left  _ -> Nothing
    Right r -> decode $ responseBody r