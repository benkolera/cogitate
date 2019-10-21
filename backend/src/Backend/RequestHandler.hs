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

requestHandler :: (forall x. Transaction mode x -> m x) -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $ runTransaction . \case
    ApiRequest_Public r -> case r of
      PublicRequest_NoOp -> pure ()

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()
