{-# LANGUAGE OverloadedStrings #-}

module AlgoliaParser where

import Control.Applicative  -- haskell-on-heroku is still GHC 7.8, needs this
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.Aeson.Types (typeMismatch, emptyArray)
import Data.Text (Text)

data Best = Best { _qid    :: !Int
                 , _qtitle :: !Text
                 , _oid    :: !Int
                 , _otitle :: !Text
                 , _getURL :: !Text
                 } deriving (Eq, Show)
instance FromJSON Best where
  parseJSON (Object o) = do
      hits <- o .: "hits"
      case hits of
        []    -> typeMismatch "Search yielded no results" emptyArray
        (q:_) -> case q of
          Question _ _ []
            -> typeMismatch "Top question has no options" emptyArray
          Question qi qt (o':_)
            -> return $
               Best qi qt (_oi o') (_ot o') (_gh o')
  parseJSON v = typeMismatch "Type mismatch parsing Result" v

data Question = Question
    { _qi :: !Int
    , _qt :: !Text
    , _os :: ![Viewpoint]
    } deriving (Eq, Show)
instance FromJSON Question where
  parseJSON (Object o) = Question
    <$> o .: "id"
    <*> (o .: "revision" >>= (.: "title"))
    <*> (o .: "viewpoints" >>= (.: "children"))
  parseJSON v = typeMismatch "Type mismatch parsing Question" v

data Viewpoint = Viewpoint
    { _oi :: !Int
    , _ot :: !Text
    , _gh :: !Text
    } deriving (Eq, Show)
instance FromJSON Viewpoint where
  parseJSON (Object o) = Viewpoint
    <$> o .: "id"
    <*> (o .: "revision" >>= (.: "title"))
    <*> (o .: "revision" >>= (.: "siteURL"))
  parseJSON v = typeMismatch "Type mismatch parsing Viewpoint" v
