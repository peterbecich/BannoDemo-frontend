module StatsThermite where

import Prelude

import Thermite as T

statsThermite :: T.Spec _ _ _ _
statsThermite = T.simpleSpec T.defaultPerformAction T.defaultRender
