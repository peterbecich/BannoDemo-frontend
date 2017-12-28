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
--import StatsThermite (statsThermite, errorThermite, StatsActions)
import StatsThermite
import RetrieveTwitterStats (retrieveTwitterStats)

-- https://github.com/ethul/purescript-react-example/blob/master/src/Main.purs#L33
-- https://stackoverflow.com/a/37353409/1007926
statsReactComponent :: forall props. T.Spec _ _ _ _ -> TwitterStats -> R.ReactClass { name :: String | props }
statsReactComponent statsSpec stats = R.createClass $ (T.createReactSpec statsSpec stats).spec


-- https://pursuit.purescript.org/packages/purescript-thermite/4.0.0/docs/Thermite#v:createReactSpec
-- https://pursuit.purescript.org/packages/purescript-react/4.4.0/docs/React#t:ReactThis
-- https://github.com/paf31/purescript-book/blob/master/text/chapter8.md#an-address-book-user-interface
-- https://github.com/ethul/purescript-react-example/blob/master/src/Main.purs#L78
-- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Types#v:htmlDocumentToDocument
-- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Window#v:document
-- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Types#v:htmlDocumentToNonElementParentNode
-- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.HTML.Types#v:htmlDocumentToNode
-- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.Node.Types#t:Node
-- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.Node.Types#t:Element
-- https://pursuit.purescript.org/packages/purescript-dom/4.13.2/docs/DOM.Node.NonElementParentNode#v:getElementById


-- https://github.com/paf31/purescript-book/blob/master/text/chapter8.md#an-address-book-user-interface
-- This illustrates an interesting point about PureScript's row-based effects: effects appearing inside rows need not be simple singletons, but can have interesting structure, and this flexibility enables some useful restrictions at compile time. If the purescript-react library did not make this restriction then it would be possible to get exceptions at runtime if we tried to write the state in the Render action, for example. Instead, such mistakes are now caught at compile time.

main :: forall e. Eff (console :: CONSOLE,
                       ajax :: AJAX,
                       dom :: DOM,
                       state :: R.ReactState ( read :: R.Read, write :: R.Write)  | e) Unit
main = do
  log "Hello sailor!"

  _ <- launchAff $ do
    eitherStats <- retrieveTwitterStats ""
    liftEff $ log $ show eitherStats
    case eitherStats of
      (Left _) -> liftEff $ T.defaultMain errorThermite unit unit
      (Right stats) -> liftEff $ renderStats stats
  pure unit      
  where
    renderStats :: forall e. TwitterStats
                   -> Eff (console :: CONSOLE,
                           ajax :: AJAX,
                           dom :: DOM,
                           state :: R.ReactState ( read :: R.Read, write :: R.Write)  | e) Unit
    renderStats stats = do
      let
        -- reactSpecDispatcher :: { spec :: R.ReactSpec _ _ _, dispatcher :: R.ReactThis _ _ -> _ -> T.EventHandler }
        reactSpecDispatcher = T.createReactSpec statsThermite stats

        reactSpecRender = reactSpecDispatcher.spec.render

        reactClass = R.createClass $ R.spec stats \ctx -> do
          -- _ <- reactSpecDispatcher.dispatcher ctx StartTimer
          reactSpecRender ctx

        reactElement = R.createFactory reactClass unit

        ui :: R.ReactElement
        ui = R.div' [ reactElement ]

      void $ liftEff $ do
        win <- window
        doc <- document win -- doc :: HTMLDocument
        let nonElementNode = htmlDocumentToNonElementParentNode doc -- node :: Node
        melement <- getElementById (NodeTypes.ElementId "foo") nonElementNode
        case melement of
          (Just element) -> RDOM.render ui element
          (Nothing) -> pure Nothing

  
