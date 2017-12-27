module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)

import Types.TwitterStats

import Network.HTTP.Affjax (AJAX)
import Thermite (defaultMain) as T


import StatsThermite (statsThermite)
import RetrieveTwitterStats (retrieveTwitterStats)

main :: forall e. Eff (console :: CONSOLE, ajax :: AJAX, dom :: DOM | e) Unit
main = do
  log "Hello sailor!"

  _ <- launchAff $ do
    stats <- retrieveTwitterStats ""
    liftEff $ log $ show stats
  
  T.defaultMain statsThermite unit unit 
  
