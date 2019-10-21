module Backend.NotifyHandler where

import qualified Data.Map.Monoidal as MMap
import Rhyolite.Backend.Listen (DbNotification (..))
import Data.Dependent.Sum      (DSum ((:=>)))

import Backend.Transaction (Transaction)
import Backend.Schema (Notification (..))
import Common.App (View (..), ViewSelector (..))
import Common.Prelude

notifyHandler :: forall a. Monoid a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler _runTransaction _msg vs = pure mempty
