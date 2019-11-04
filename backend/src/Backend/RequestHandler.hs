module Backend.RequestHandler where

import Database.Beam
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Backend.SQL.BeamExtensions as Ext
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (PrivateRequest (..), PublicRequest (..))
import Common.Schema
import Common.Prelude

requestHandler :: (forall x. Transaction mode x -> m x) -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $ runTransaction . \case
    ApiRequest_Public r -> case r of
      PublicRequest_NoOp -> pure ()
      PublicRequest_CreateEntry title body -> runQuery $ do
        [entryT] <- Ext.runInsertReturningList $ insert (_dbEntry db) $ (insertExpressions
          [ Entry
            { _entryId = default_
            , _entryTitle = val_ title
            , _entryText = val_ body
            }
          ])
        pure (EntryId $ _entryId entryT)

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()
