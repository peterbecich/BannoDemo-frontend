module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either
import Data.Maybe
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, htmlDocumentToNode)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types as NodeTypes

import Partial.Unsafe (unsafePartial)

import React as R
import React.DOM as R
import ReactDOM as RDOM

import Types.TwitterStats

import Network.HTTP.Affjax (AJAX)
--import Thermite (defaultMain, createReactSpec) as T
import Thermite as T

import Types.TwitterStats (TwitterStats)
import StatsThermite (statsThermite, errorThermite)
import RetrieveTwitterStats (retrieveTwitterStats)

-- https://pursuit.purescript.org/packages/purescript-thermite/4.0.0/docs/Thermite#v:createReactSpec
-- https://github.com/ethul/purescript-react-example/blob/master/src/Main.purs#L33
-- https://stackoverflow.com/a/37353409/1007926
statsReactComponent :: forall props. T.Spec _ _ _ _ -> TwitterStats -> R.ReactClass { name :: String | props }
statsReactComponent statsSpec stats = R.createClass $ (T.createReactSpec statsSpec stats).spec

main :: forall e. Eff (console :: CONSOLE, ajax :: AJAX, dom :: DOM | e) Unit
main = do
  log "Hello sailor!"

  _ <- launchAff $ do
    eitherStats <- retrieveTwitterStats ""
    liftEff $ log $ show eitherStats
    case eitherStats of
      (Left _) -> liftEff $ T.defaultMain errorThermite unit unit
      (Right stats) -> do
        let
          reactSpecDispatcher = T.createReactSpec statsThermite stats
          reactClass = R.createClass $ reactSpecDispatcher.spec
          reactElement = R.createFactory reactClass unit
          ui :: R.ReactElement
          ui = R.div' [ reactElement ]

        -- https://github.com/ethul/purescript-react-example/blob/master/src/Main.purs#L78
        -- liftEff $ T.defaultMain statsThermite stats unit
        -- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Types#v:htmlDocumentToDocument
        void $ liftEff $ do
          win <- window
          -- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Window#v:document
          doc <- document win -- doc :: HTMLDocument
          -- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Types#v:htmlDocumentToNonElementParentNode
          -- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Types#v:htmlDocumentToNode
          let nonElementNode = htmlDocumentToNonElementParentNode doc -- node :: Node
          -- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.Node.Types#t:Node
          -- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.Node.Types#t:Element
          -- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.Node.NonElementParentNode#v:getElementById
          melement <- getElementById (NodeTypes.ElementId "foo") nonElementNode
          case melement of
            (Just element) -> RDOM.render ui element
            (Nothing) -> pure Nothing
          -- RDOM.render ui (unsafePartial (fromJust melement))

  pure unit
  
  
