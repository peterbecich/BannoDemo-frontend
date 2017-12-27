module StatsThermite where

import Prelude

import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Trans.Class (lift)

import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Newtype (wrap)

import Data.Either

import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP

import RetrieveTwitterStats (retrieveTwitterStats)

import Types.TwitterStats

statsThermite :: T.Spec _ _ _ _
statsThermite = T.simpleSpec statsAction statsRender
  where
    statsAction :: T.PerformAction _ _ _ _
    statsAction _ _ _ = tickLoop
    
    statsRender :: T.Render TwitterStats _ _
    -- statsRender _ _ _ _ = [ R.p'  [ R.text "Tweet count: " ] ]
    statsRender _ _ (TwitterStats {tweetCount, emojiTweetCount, urlTweetCount, picTweetCount, hashtagTweetCount}) _ =
      [ R.p'  [ R.text "Tweet count: ", R.text (show tweetCount) ]
      , R.p'  [ R.text "Emoji tweet count: ", R.text (show emojiTweetCount) ]
      ]

tickLoop :: T.CoTransformer _ _ _ _
tickLoop = do
  _ <- liftEff $ log $ show "tick loop"
  _ <- lift $ delay (wrap (1000.0))
  _ <- liftEff $ launchAff $ do
       eitherStats <- retrieveTwitterStats ""
       liftEff $ log $ show $ eitherStats
       -- case eitherStats of
       --   (Left _) -> T.modifyState(id)
       --   (Right stats') -> T.modifyState(\_ -> stats')
  tickLoop

errorThermite :: T.Spec _ _ _ _
errorThermite = T.simpleSpec T.defaultPerformAction errorRender
  where
    errorRender :: T.Render _ _ _
    errorRender _ _ _ _ = [ R.p' [ R.text "no statistics retrieved" ] ]

