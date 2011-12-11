
{-# LANGUAGE TupleSections #-}
module Location
  ( LE
  , L(..)
  , withLoc
  , keepLoc
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Text.Parsec

import Message

type LE = StateT SourcePos E

data L a = L
  { val :: a
  , loc :: SourcePos
  } deriving (Eq, Show)

instance Functor L where
  fmap f (L val loc) = L (f val) loc

vals :: (Functor f) => f (L a) -> f a
vals = fmap val

withLoc :: (a -> LE b) -> L a -> E (L b)
withLoc f x = fmap (uncurry L) $ runStateT (f $ val x) $ loc x

keepLoc :: (a -> E b) -> L a -> E (L b)
keepLoc f x = f (val x) >>= return . (flip L $ loc x)

