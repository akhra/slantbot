{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slantbot.Reddit.Subreddits.Responses where

import           Data.Monoid
import           Data.Text
import           Slantbot.Algolia

response :: Maybe Question -> Text -> Text
response question utm = responseBody question utm <> responseFooter

responseBody :: Maybe Question -> Text -> Text
responseBody q utm = case q of
  Just q'@Question{questionOptions=(o:_)}
    -> responseWith q' o utm
  _ -> responseNoResult

responseWith :: Question -> Option -> Text -> Text
responseWith q o utm
  = "Hi there! It looks like you're asking:  \n"
  <> "[*" <> questionTitle q <> "*](" <> qURL <> ")\n\n"
  <> "The most-recommended option is " <> option <> ".  \n"
  <> "If you want more info, you can:\n\n"
  <> "- [See its pros and cons]" <> "(" <> oURL <> ")\n\n"
  <> "- [See the other options]" <> "(" <> qURL <> ")"
  where
    url = "http://www.slant.co/topics/" <> pack (show $ questionID q)
    qURL = url <> utm
    oURL = url <> "/viewpoints/" <> pack (show $ optionID o) <> utm
    option = if optionGet o == empty
             then "**" <> optionTitle o <> "**"
             else "[**" <> optionTitle o <> "**](" <> optionGet o <> ")"

responseNoResult :: Text
responseNoResult
  = "Sorry, I couldn't find anything for that query."
  <> " I'll just sit in the corner and contemplate my failure."

responseFooter :: Text
responseFooter
  =  "\n\n---\n"
  <> "^(I am a bot! For safety, replies to this comment will be ignored."
    <> " Direct messages will be checked by a human.)"
