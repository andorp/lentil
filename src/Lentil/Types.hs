{-# LANGUAGE RankNTypes #-}

module Lentil.Types (
    Entity(..)
  , EntityLens
  , EntityLens'
  , MonadLens
  , MonadLens'
  , LifeCycle(..)
  , module T
  ) where

import Control.Lens (LensLike)
import Data.Functor.Compose as T (Compose(..))


type C = Compose

type MonadLens m f s t a b = LensLike (C m f) s t a b

type MonadLens' m f s a = MonadLens m f s s a a

type EntityLens backend s t a b
  = forall f . (Functor f, Traversable f)
             => MonadLens backend f s t a b

type EntityTraversal backend s t a b
  = forall f . (Applicative f, Traversable f)
             => MonadLens backend f s t a b

type EntityLens' backend s a = EntityLens backend s s a a

data LifeCycle backend key entity = LifeCycle {
    saveEntity   :: entity     -> backend (key entity)
  , deleteEntity :: key entity -> backend ()
  }

data Entity backend key entity result = Entity {
    loadEntity   :: key entity           -> backend result
  , updateEntity :: key entity -> result -> backend ()
  }
