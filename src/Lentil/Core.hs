{-# LANGUAGE RankNTypes          #-}
module Lentil.Core (
    get
  , over
  , over'
  , set
  , set'
  , entityLens
  , module Lentil.Types
  ) where

import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Traversable as Traversable (mapM)

import Lentil.Types


get :: Monad m => MonadLens m (Const a) s t a a -> s -> m a
get c = fmap (fmap getConst . getCompose) . c $ (Compose . pure . Const)

over :: Monad m => MonadLens m Identity s t a b -> (a -> m b) -> s -> m t
over c f = fmap (fmap runIdentity . getCompose). c $ (Compose . fmap Identity . f)

over' :: Monad m => MonadLens m Identity s t a b -> (a -> b) -> s -> m t
over' c f = over c (pure . f)

set :: Monad m => MonadLens m Identity s t a b -> m b -> s -> m t
set c mb = over c (const mb)

set' :: Monad m => MonadLens m Identity s t a b -> b -> s -> m t
set' c b = over' c (const b)

entityLens :: Monad backend
           => Entity backend key entity result -> EntityLens' backend (key entity) result
entityLens handler focus entityKey = Compose $ do
  e  <- loadEntity handler entityKey
  fe <- getCompose (focus e)
  te <- Traversable.mapM (updateEntity handler entityKey) fe
  pure $ (const entityKey) <$> te
