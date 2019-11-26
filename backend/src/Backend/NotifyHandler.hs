module Backend.NotifyHandler where

import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Rhyolite.Backend.Listen (DbNotification (..))
import Data.Dependent.Sum      (DSum ((:=>)))

import Backend.Transaction (Transaction)
import Backend.Schema (Notification (..))
import Common.App (View (..), ViewSelector (..))
import Common.Prelude

notifyHandler :: forall a. Monoid a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler _runTransaction msg vs = case _dbNotification_message msg of
  Notification_Entry :=> Identity (_change, eId) ->
    pure $ mempty
      { _view_entryIds = fmap (,Set.singleton eId)  $ _viewSelector_entryIds vs
      }
