module Slantbot.Algolia where

import           Algolia.Query
import           Algolia.Response
import           Data.Aeson
import           Data.Char
import           Data.Maybe
import           Data.Text            (Text)

questionSearch :: AlgoliaProfile -> String -> IO (Maybe Question)
questionSearch _ [] = return Nothing
questionSearch profile query = do
  response <- algoliaQuery groomChars groomWords profile query
  return $ case response of
    Just r -> userData <$> (listToMaybe . hits $ r)
    _      -> Nothing
  where
    groomChars = fmap toLower . filter (not . (=='?'))
    groomWords = filter $ not . flip elem
                   [ "", "what", "are", "is", "the", "best", "a"
                   , "an", "for", "in", "of", "on", "to", "with" ]

-- TODO: More complete schemas; also perhaps a dash of lens.
data Question = Question
    { questionID      :: !Int
    , questionVotes   :: !Int
    , questionTitle   :: !Text
    , questionOptions :: ![Option]
    } deriving (Eq, Show)
instance FromJSON Question where
  parseJSON (Object v) = Question
    <$> v.:"id"
    <*> v.:"totalVotes"
    <*> (v.:"revision" >>= (.:"title"))
    <*> (v.:"viewpoints" >>= (.:"children"))
  parseJSON _ = mempty

data Option = Option
    { optionID    :: !Int
    , optionVotes :: !Int
    , optionTitle :: !Text
    , optionGet   :: !Text
    } deriving (Eq, Show)
instance FromJSON Option where
  parseJSON (Object v) = Option
    <$> v.:"id"
    <*> (v.:"votes" >>= (.:"count"))
    <*> (v.:"revision" >>= (.:"title"))
    <*> (v.:"revision" >>= (.:"siteURL"))
  parseJSON _ = mempty
