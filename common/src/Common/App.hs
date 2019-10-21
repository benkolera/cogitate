{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-} -- For deriveJSONGADT
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.App where

import Control.Lens.TH (makeLenses)
import Data.Aeson (parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Align (Align (nil), Semialign (alignWith))
import qualified Data.Align as Align
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Witherable (Witherable (wither))
import Data.MonoidMap (MonoidMap)
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount (..))
import Reflex.Patch (Additive, Group (negateG))
import Rhyolite.App (PositivePart (positivePart), standardPositivePart)

import Common.Prelude
import Common.Schema

data PublicRequest a where
  PublicRequest_NoOp :: PublicRequest ()
deriving instance Show a => Show (PublicRequest a)

fmap concat $ sequence
  [ deriveJSONGADT ''PublicRequest
  , deriveArgDict ''PublicRequest
  ]

data PrivateRequest a where
  PrivateRequest_NoOp :: PrivateRequest ()
fmap concat $ sequence
  [ deriveJSONGADT ''PrivateRequest
  , deriveArgDict ''PrivateRequest
  ]
deriving instance Show a => Show (PrivateRequest a)

-- ORPHANS
-- https://github.com/isomorphism/these/pull/121
deriving newtype instance Semialign Option
deriving newtype instance Align Option

-- https://github.com/fumieval/witherable/pull/43
instance Filterable Option where
  mapMaybe f = (>>= Option . f)
  {-# INLINE mapMaybe #-}
instance Witherable Option where
  wither f (Option x) = Option <$> wither f x
  {-# INLINE wither #-}

data ViewSelector a = ViewSelector
  {
  }
  deriving (Eq, Functor, Generic)
deriveJSON Json.defaultOptions 'ViewSelector
makeLenses 'ViewSelector
instance Semigroup a => Semigroup (ViewSelector a) where
  _a <> _b = ViewSelector
instance Semigroup a => Monoid (ViewSelector a) where
  mempty = ViewSelector
  mappend = (<>)
instance Semialign ViewSelector where
  alignWith _f _a _b = ViewSelector
  zipWith _f _a _b = ViewSelector
instance Align ViewSelector where
  nil = ViewSelector
instance (Group a) => Group (ViewSelector a) where
  negateG = fmap negateG
instance (Semigroup a) => Additive (ViewSelector a)
instance (Ord k) => PositivePart (ViewSelector (MonoidMap k SelectedCount)) where
  positivePart x =
    let u = mapMaybe standardPositivePart x
    in if u == mempty then Nothing else Just u
instance Filterable ViewSelector where
  mapMaybe _f _x = ViewSelector
instance (Monoid a) => Query (ViewSelector a) where
  type QueryResult (ViewSelector a) = View a
  crop _vs _v = View

data View a = View
  deriving (Eq, Foldable, Functor, Generic)
deriveJSON Json.defaultOptions 'View
makeLenses 'View
instance Monoid a => Semigroup (View a) where
  _a <> _b = View
instance Monoid a => Monoid (View a) where
  mempty = View 
  mappend = (<>)
instance Filterable View where
  mapMaybe _f _x = View
