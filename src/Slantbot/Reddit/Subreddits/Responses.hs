{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slantbot.Reddit.Subreddits.Responses where

import           Data.Monoid
import           Data.Text        (Text, pack, unpack, strip)
import qualified Data.Text        as T
import           Slantbot.Algolia

response :: Maybe Question -> Text -> Text
response question utm = responseBody question utm <> responseFooter

responseBody :: Maybe Question -> Text -> Text
responseBody q utm = case q of
  Just q' -> responseWith q' utm
  _       -> responseNoResult utm

responseFooter :: Text
responseFooter
  =  "\n\n---\n\
     \^(I am a bot! For safety, replies to this comment will be ignored. \
     \Private messages are forwarded to a human.)"

responseNoResult :: Text -> Text
responseNoResult utm
  = "Sorry, I couldn't find anything for that query. I've really dropped the \
    \ball on this one... if you'll excuse me, I feel the need to go sit \
    \quietly in a corner.\n\n\
    \If you want, though, you could try asking this question directly on \
    \[Slant](http://www.slant.co/"<>utm<>"), where I get my data. It might \
    \save me some embarrassment the next time someone asks about this!"

responseWith :: Question -> Text -> Text
responseWith q utm
  = "Hi there! It looks like you're asking:  \n"
  <> "[*" <> (strip $ questionTitle q) <> "*](" <> qURL <> ")\n\n"
  <> intro <> (T.concat $ take 3 options) <> note <> "\n\n" <> outro
  where
    url = "http://www.slant.co/topics/" <> pack (show $ questionID q)
    qURL = url <> utm
    options = format <$> questionOptions q
    format o = "\n\n" <> prefix (optionID o) <> title o
    prefix n = "- [(s)](" <> url <> "/viewpoints/" <> pack (show n)
               <> utm <> ") / "
    optGet o = strip $ optionGet o
    title o = case unpack $ optGet o of
                'h':'t':'t':'p':_
                  -> "[" <> t <> "](" <> optGet o <> ")"
                _ -> t
            where
              t = "**" <> (strip $ optionTitle o) <> "**"
    ocount = length options
    intro | ocount == 0 = ""
          | ocount == 1 = "I have one recommended option: "
          | ocount == 2 = "I have two recommended options: "
          | ocount == 3 = "I have three recommended options: "
          | otherwise   = "My three most recommended options are: "
    note | ocount == 0 = ""
         | otherwise
           = "\n\n*(s) goes to the option's pros and cons on Slant.*"
    outro | ocount == 0 = oEmpty
          | ocount < 4  = oShort
          | ocount == 4 = oPlus1
          | otherwise   = oFull
    oEmpty
      = "Unfortunately, although that question has been asked, no options \
        \have been suggested yet. If anyone has a recommendation, adding it \
        \to Slant via the question link above would help avoid this tragic \
        \outcome in the future."
    oSingle | ocount == 1 = "Kind of looks lonely there, all by itself... i"
            | otherwise   = "I"
    oShort
      = "At the moment, that's all I've got. " <> oSingle <> "f anyone has \
        \another recommendation, adding it to Slant via the question link \
        \above would help improve my answer next time this is asked."
    oPlus1
      = "There's one more option recommended for this question, but I'm \
        \programmed to show a maximum of three. Hey, rules are rules! You \
        \can click the question link above to see it, or add your own."
    oFull
      = "In addition to these, another " <> (pack . show $ ocount - 3) <>
        " options have been suggested. To see them too, or to make your own \
        \suggestion, follow the question link above."
