{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Lens

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Text.Lazy as TL
import GHCJS.DOM.Document (createElement)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Types (liftJSM)
import qualified Lucid                  as L
import Data.Semigroup (getFirst)
import qualified Data.Set as Set
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest(ApiRequest_Public))
import Rhyolite.Frontend.App (RhyoliteWidget, functorToWire, runObeliskRhyoliteWidget, watchViewSelector)
import qualified Text.MMark as MMark

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
  el "title" $ text "Cogitate"
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
      elAttr "a" ("class"=:"navbar-item" <> "href"=:"https://bulma.io") $ text "Cogitate"

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
  :: forall m t js. (Prerender js t m, HasApp t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m ()
appWidget = do
  el "h1" $ text "Butts"
  eClick <- button "Click me"
  _ <- requesting $ ApiRequest_Public (PublicRequest_CreateEntry "butts" "# Butts 2\n\ntetaotaota") <$ eClick
  dEntryIds <- watchEntryIds
  dEntries :: Dynamic t (MMap.MonoidalMap EntryId Entry) <- watchEntries (fromMaybe Set.empty <$> dEntryIds)
  void $ list (MMap.getMonoidalMap <$> dEntries) $ \dEntry -> do
    entryContent (Just <$> dEntry)

entryContent
  :: forall t js m
  .  ( DomBuilder t m
     , Prerender js t m
     )
  => Dynamic t (Maybe Entry)
  -> m ()
entryContent dEntry = prerender_ (text "Rendering Document...") $ do
  let dHtml = fromMaybe "" . (fmap (markDownToHtml5 . _entryText)) <$> dEntry
  elClass "div" "row entry-content" $ do
    d <- askDocument
    -- We have to sample the initial value to set it on creation
    htmlT <- sample . current $ dHtml
    e <- liftJSM $ do
      -- This wont execute scripts, but will allow users to XSS attack through
      -- event handling javascript attributes in any raw HTML that is let
      -- through the markdown renderer. But this is the simplest demo that
      -- mostly works. See https://github.com/qfpl/reflex-dom-template for a
      -- potentially more robust solution (we could filter out js handler attrs
      -- with something like that).
      -- It's worth noting that the react demo app does exactly what this does:
      -- https://github.com/gothinkster/react-redux-realworld-example-app/blob/master/src/components/Article/index.js#L60
      e <- createElement d ("div" :: String)
      setInnerHTML e htmlT
      pure e
    -- And make sure we update the html when the article changes
    performEvent_ $ (liftJSM . setInnerHTML e) <$> updated dHtml
    -- Put out raw element into our DomBuilder
    placeRawElement e

markDownToHtml5 :: Text -> Text
markDownToHtml5 t =
  case MMark.parse "" t of
    Left _  -> ""
    Right r -> TL.toStrict . L.renderText . MMark.render $ r

watchEntryIds
  :: (HasApp t m, MonadHold t m, MonadFix m)
  =>  m (Dynamic t (Maybe (Set.Set EntryId)))
watchEntryIds = do
  res :: Dynamic t (View SelectedCount) <- watchViewSelector $ constDyn $ (mempty :: ViewSelector SelectedCount)
    { _viewSelector_entryIds = pure 1 }
  pure $ ffor res $ \r -> r ^? view_entryIds . to getOption . _Just . _2

watchEntries
  :: (HasApp t m, MonadHold t m, MonadFix m)
  => Dynamic t (Set EntryId)
  -> m (Dynamic t (MMap.MonoidalMap EntryId Entry))
watchEntries dEntryIds = do
  res :: Dynamic t (View SelectedCount) <- watchViewSelector $ ffor dEntryIds $ \entryIds -> (mempty :: ViewSelector SelectedCount)
    { _viewSelector_entries = MMap.fromList . fmap (,1) . Set.toList $ entryIds }
  pure . ffor res $ \r -> fmap (^. _2 . to getFirst) (r ^. view_entries)

watchEntry
  :: (HasApp t m, MonadHold t m, MonadFix m)
  => Dynamic t EntryId
  -> m (Dynamic t (Maybe Entry))
watchEntry dEntryId = do
  res :: Dynamic t (View SelectedCount) <- watchViewSelector $ ffor dEntryId $ \entryId -> (mempty :: ViewSelector SelectedCount)
    { _viewSelector_entries = MMap.singleton entryId 1 }
  pure . ffor2 dEntryId res $ \entryId r -> r ^? view_entries . ix entryId . _2 . to getFirst
