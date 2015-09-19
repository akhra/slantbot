module Main where

import           Control.Concurrent.Async
import           Slantbot.Config
import qualified Slantbot.Reddit          as Reddit
-- import qualified Slantbot.Twitter         as Twitter

main :: IO ()
main = do
  config <- getConfig
  threads <- mapM async
    [ Reddit.runBot $ reddit config
--    , Twitter.runBot $ twitter config
    ]
  mapM_ wait threads
