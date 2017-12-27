module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)

import Thermite (defaultMain) as T


import StatsThermite (statsThermite)

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  log "Hello sailor!"

  T.defaultMain statsThermite unit unit 
  
