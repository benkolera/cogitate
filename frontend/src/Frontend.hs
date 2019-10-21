{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad.Fix (MonadFix)
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest)
import Rhyolite.Frontend.App (RhyoliteWidget, functorToWire, runObeliskRhyoliteWidget)

import Obelisk.Generated.Static

import Common.App
import Common.Route
import Common.Prelude
import Common.Schema


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headSection
  , _frontend_body = runAppWidget $ skeleton $ subRoute_ $ \case
      FrontendRoute_Main -> appWidget 
  }

headSection :: DomBuilder t m => m ()
headSection = do
  elAttr "meta" ("charset"=:"utf-8") blank
  elAttr "meta" ("name"=:"viewport" <> "content"=:"width=device-width, initial-scale=1") blank
  elAttr "link" ("rel"=:"stylesheet" <> "type"=:"text/css" <> "href"=: static @"css/bulma.css") blank
  el "title" $ text "Something"
  elAttr "script" ("defer"=:"defer"<> "src"=:"https://use.fontawesome.com/releases/v5.3.1/js/all.js") blank

runAppWidget ::
  ( HasConfigs m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  , Prerender x t m
  )
  => RoutedT t (R FrontendRoute) (RhyoliteWidget (ViewSelector SelectedCount) (ApiRequest () PublicRequest PrivateRequest) t m) a
  -> RoutedT t (R FrontendRoute) m a
runAppWidget = runObeliskRhyoliteWidget
  functorToWire
  "common/route"
  checkedFullRouteEncoder
  (BackendRoute_Listen :/ ())


skeleton
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, SetRoute t (R FrontendRoute) m)
  => m a -> m a
skeleton body = do
  navBar
  a <- elClass "section" "section" body
  footer
  pure a


navBar
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, SetRoute t (R FrontendRoute) m)
  => m ()
navBar =
  elAttr "nav" ("class"=:"navbar" <> "role"=:"navigation" <> "aria-label"=:"main navigation") $ do
    divClass "navbar-brand" $
      elAttr "a" ("class"=:"navbar-item" <> "href"=:"https://bulma.io") $ text "Tree of Life"

    (burgerEl, ()) <- elAttr' "a" ("role"=:"button" <> "class"=:"navbar-burger burger" <> "aria-label"=:"menu") $ do
      let ln = elAttr "span" ("aria-hidden"=:"true") blank
      ln *> ln *> ln

    menuIsActive <- toggle False $ domEvent Click burgerEl

    elDynAttr "div" (ffor menuIsActive $ \active -> "class"=:("navbar-menu" <> if active then " is-active" else "")) $ do
      divClass "navbar-start" $ do
        (homeEl, _) <- elClass' "a" "navbar-item" $ text "Home"
        setRoute $ FrontendRoute_Main :/ () <$ domEvent Click homeEl

footer :: DomBuilder t m => m ()
footer =
  elClass "footer" "footer" $
    divClass "content has-text-centered" $
      el "p" $ do
        el "strong" (text "Cogitate") *> text " by Ben Kolera. The source code is licensed "
        elAttr "a" ("href"=:"http://opensource.org/licenses/mit-license.php") (text "MIT")

type HasApp t m =
  ( MonadQuery t (ViewSelector SelectedCount) m
  , Requester t m
  , Request m ~ ApiRequest () PublicRequest PrivateRequest
  , Response m ~ Identity
  )

appWidget
  :: forall m t. (HasApp t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m ()
appWidget = do
  el "h1" $ text "Butts"
  pure ()
