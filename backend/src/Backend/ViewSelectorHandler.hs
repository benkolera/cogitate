{-# LANGUAGE FlexibleContexts #-}
module Backend.ViewSelectorHandler where

import Backend.Transaction (Transaction)
import Common.App (View (..), ViewSelector (..))
import Common.Prelude ()

viewSelectorHandler :: (Monoid a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = if vs == mempty then pure mempty else runTransaction $ do
  pure $ View
