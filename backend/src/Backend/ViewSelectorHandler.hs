{-# LANGUAGE FlexibleContexts #-}
module Backend.ViewSelectorHandler where

import Data.Semigroup (First(First))
import Database.Beam
import Database.Beam.Backend (BeamSqlBackend)

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (View (..), ViewSelector (..))
import Common.Schema
import Common.Prelude

viewSelectorHandler :: (Eq a, Monoid a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = if vs == mempty then pure mempty else runTransaction $ do
  entries <- ifor (_viewSelector_entries vs) $ \entryId a ->
    (a,) <$> getEntry entryId
  pure $ View
    { _view_entries = mapMaybe (\(a,me) -> (a,) . First <$> me) entries }

getEntry :: EntryId -> Transaction mode (Maybe Entry)
getEntry entryId = fmap headMay $
  runQuery $ runSelectReturningList $ select $ do
    entry <- all_ (_dbEntry db)
    guard_ (_entryId entry ==. val_ (unEntryId entryId))
    pure entry
