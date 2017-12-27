module RetrieveTwitterStats where

import Prelude
import Data.Maybe
import Data.Either
import Control.Monad.Eff.Class
import Control.Monad.Except
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List.Types

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Generic.Rep


import Network.HTTP.Affjax (get, post, Affjax, AJAX, AffjaxResponse)
import Control.Monad.Aff

import Types.TwitterStats

retrieveTwitterStats :: forall e. String
                     ->  Aff (ajax :: AJAX, console :: CONSOLE | e) (Either (NonEmptyList ForeignError) TwitterStats)
retrieveTwitterStats host = do
  res <- get (host <> "/stats") :: Affjax (console :: CONSOLE | e) String
  liftEff $ log $ res.response
  -- maybeStats <- do
  --   es <- runExcept $ decodeJSON res.response
  --   -- liftEff $ log $ show es
  --   hush es
  -- maybeStats
  pure $ runExcept $ decodeJSON res.response
