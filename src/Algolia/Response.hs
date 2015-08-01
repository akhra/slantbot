{-# LANGUAGE TemplateHaskell #-}

module Algolia.Response where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text           (Text)

-- TODO: Lens! Many lens! That's 50 ADT minus!
data AlgoliaResponse a = AlgoliaResponse
  { hits              :: ![AlgoliaHit a]
  , nbHits            :: !Int
  , page              :: !Int
  , nbPages           :: !Int
  , hitsPerPage       :: !Int
  , processingTimeMS  :: !Int
  , query             :: !Text
  , params            :: !Text
  , queryAfterRemoval :: !(Maybe Text)
  , parsedQuery       :: !(Maybe Text)
  , index             :: !(Maybe Text)
  , serverUsed        :: !(Maybe Text)
  , timeoutCounts     :: !(Maybe Bool)
  , timeoutHits       :: !(Maybe Bool)
  } deriving (Show)
-- Can't derive this; probably because of weirdness in AlgoliaHit.
instance (FromJSON a) => FromJSON (AlgoliaResponse a) where
  parseJSON (Object v) = AlgoliaResponse
    <$> v.:"hits"
    <*> v.:"nbHits"
    <*> v.:"page"
    <*> v.:"nbPages"
    <*> v.:"hitsPerPage"
    <*> v.:"processingTimeMS"
    <*> v.:"query"
    <*> v.:"params"
    <*> v.:?"queryAfterRemoval"
    <*> v.:?"parsedQuery"
    <*> v.:?"index"
    <*> v.:?"serverUsed"
    <*> v.:?"timeoutCounts"
    <*> v.:?"timeoutHits"
  parseJSON _ = mempty

data AlgoliaHit a = AlgoliaHit
  { rankingInfo :: !(Maybe AlgoliaRankingInfo)
  , userData    :: !a
  } deriving (Show)
-- Algolia stuffs the optional ranking info INSIDE the user-data object...
instance (FromJSON a) => FromJSON (AlgoliaHit a) where
  parseJSON o@(Object v) = AlgoliaHit
    <$> v.:?"_rankingInfo"
    <*> parseJSON o
  parseJSON _ = mempty

data AlgoliaRankingInfo = AlgoliaRankingInfo
  { nbTypos           :: !Int
  , firstMatchedWord  :: !Int
  , proximityDistance :: !Int
  , userScore         :: !Int
  , geoDistance       :: !Int
  , geoPrecision      :: !Int
  , nbExactWords      :: !Int
  , words             :: !Int
  } deriving (Show)
$(deriveJSON defaultOptions ''AlgoliaRankingInfo)
