module StatsThermite where

import Prelude

import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP


import Types.TwitterStats

statsThermite :: T.Spec _ _ _ _
statsThermite = T.simpleSpec T.defaultPerformAction statsRender
  where
    statsRender :: T.Render TwitterStats _ _
    -- statsRender _ _ _ _ = [ R.p'  [ R.text "Tweet count: " ] ]
    statsRender _ _ (TwitterStats {tweetCount, emojiTweetCount, urlTweetCount, picTweetCount, hashtagTweetCount}) _ =
      [ R.p'  [ R.text "Tweet count: ", R.text (show tweetCount) ]
      , R.p'  [ R.text "Emoji tweet count: ", R.text (show emojiTweetCount) ]
      ]


errorThermite :: T.Spec _ _ _ _
errorThermite = T.simpleSpec T.defaultPerformAction errorRender
  where
    errorRender :: T.Render _ _ _
    errorRender _ _ _ _ = [ R.p' [ R.text "no statistics retrieved" ] ]

