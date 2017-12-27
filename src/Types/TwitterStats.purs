module Types.TwitterStats where

import Prelude

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep

import Types.DateTimeWrapped

newtype TwitterStats = TwitterStats
                       { -- serverStartTimestamp :: DateTime'
                       -- , statsTimestamp :: DateTime'
                       tweetCount :: Int
                       , emojiTweetCount :: Int
                       , urlTweetCount :: Int
                       , picTweetCount :: Int
                       , hashtagTweetCount :: Int
                       }

derive instance genericTwitterStats :: Generic TwitterStats _

instance showTwitterStats :: Show TwitterStats where 
  show (TwitterStats stats) =
    --show stats.serverStartTimestamp <> " " <> show stats.statsTimestamp <> " " <> show stats.tweetCount
    show stats.tweetCount


instance decodeTwitterStats :: Decode TwitterStats where
  decode = genericDecode (
    defaultOptions {
       unwrapSingleConstructors = true
       }
    )




