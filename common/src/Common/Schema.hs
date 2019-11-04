{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Schema where

import qualified Data.Aeson as Json
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, PrimaryKey, Table (primaryKey))
import Database.Beam.Backend.SQL.Types (SqlSerial)
import GHC.Generics (Generic)

import Common.Prelude

-------------------------------------------------------------------------------
data EntryT f = Entry
  { _entryId :: Columnar f (SqlSerial Int)
  , _entryTitle :: Columnar f Text
  , _entryText :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table EntryT where
  data PrimaryKey EntryT f = EntryId { unEntryId :: Columnar f (SqlSerial Int) } deriving stock Generic deriving anyclass Beamable
  primaryKey = EntryId . _entryId

type Entry = EntryT Identity
deriving instance Eq Entry
deriving instance Show Entry
instance FromJSON Entry
instance ToJSON Entry

type EntryId = PrimaryKey EntryT Identity
deriving instance Eq EntryId
deriving instance Ord EntryId
deriving instance Show EntryId
instance FromJSON EntryId
instance ToJSON EntryId
instance Json.ToJSONKey EntryId
instance Json.FromJSONKey EntryId
-------------------------------------------------------------------------------
