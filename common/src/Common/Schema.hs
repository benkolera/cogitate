{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Schema where

import Data.Text (Text)
import Database.Beam (Beamable, Columnar, PrimaryKey, Table (primaryKey))
import GHC.Generics (Generic)

import Common.Prelude

-------------------------------------------------------------------------------
data TopicT f = Topic
  { _bookId :: Columnar f Int
  , _bookName :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table TopicT where
  data PrimaryKey TopicT f = TopicId { unTopicId :: Columnar f Int } deriving stock Generic deriving anyclass Beamable
  primaryKey = TopicId . _bookId

type TopicId = PrimaryKey TopicT Identity
deriving instance Eq TopicId
deriving instance Ord TopicId
deriving instance Show TopicId
instance FromJSON TopicId
instance ToJSON TopicId
-------------------------------------------------------------------------------
