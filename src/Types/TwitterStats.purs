module Types.TwitterStats where

import Prelude

import Data.Array
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep
import Data.Traversable

import Types.DateTimeWrapped

newtype TwitterStats = TwitterStats
                       { -- serverStartTimestamp :: DateTime'
                       -- , statsTimestamp :: DateTime'
                       tweetCount :: Number
                       , emojiTweetCount :: Number
                       , emojiPercentage :: Number
                       , urlTweetCount :: Number
                       , urlPercentage :: Number
                       , picTweetCount :: Number
                       , picPercentage :: Number
                       , hashtagTweetCount :: Number
                       , hashtagPercentage :: Number
                       }

derive instance genericTwitterStats :: Generic TwitterStats _

instance showTwitterStats :: Show TwitterStats where 
  -- show :: TwitterStats -> String
  show (TwitterStats stats) = show mArr
    where
      -- mArr :: Maybe (Array Number)
      -- mArr = sequence ([stats.tweetCount, stats.emojiTweetCount, stats.emojiPercentage])
      -- mArr :: (Array (Maybe Number))
      -- mArr = sequence ([stats.tweetCount, stats.emojiTweetCount, stats.emojiPercentage])
      mArr :: Array Number
      mArr = [stats.tweetCount
             , stats.emojiTweetCount
             , stats.emojiPercentage
             , stats.urlTweetCount
             , stats.urlPercentage
             , stats.picTweetCount
             , stats.picPercentage
             , stats.hashtagTweetCount
             , stats.hashtagPercentage
             ]
             




instance decodeTwitterStats :: Decode TwitterStats where
  decode = genericDecode (
    defaultOptions {
       unwrapSingleConstructors = true
       }
    )




