module CustomResource.Prelude (module Exports, identity) where

import Control.Applicative as Exports
  ( Applicative
  , (<*>)
  , empty
  , pure
  )

import Control.Monad as Exports
  ( Monad
  , (<=<)
  , (=<<)
  , (>>)
  , void
  )

import Control.Monad.IO.Class as Exports
  ( MonadIO
  , liftIO
  )

import Data.Bool as Exports
  ( Bool
    ( False
    , True
    )
  , (&&)
  , (||)
  , not
  , otherwise
  )

import Data.Either as Exports
  ( Either
    ( Left
    , Right
    )
  , either
  )

import Data.Eq as Exports
  ( Eq
  , (/=)
  , (==)
  )

import Data.Function as Exports
  ( ($)
  , (.)
  , const
  )

import Data.Functor as Exports
  ( Functor
  , (<$>)
  )

import Data.Maybe as Exports
  ( Maybe(Just)
  , fromMaybe
  , listToMaybe
  , maybe
  )

import Data.Ord as Exports
  ( Ord
  , (<)
  , (<=)
  , (>)
  , (>=)
  )

import Data.Semigroup as Exports
  ( Semigroup
  , (<>)
  )

import Data.Text as Exports
  ( Text
  )

import Data.Text.Conversions as Exports
  ( ToText
  , convertText
  , toText
  )

import Text.Show as Exports
  ( Show
  , show
  )

-- | Identity function
--
-- We do not re-export Data.Function.id to avoid common name clashes with serial
-- colums in databases that are commonly named `id`.
identity :: a -> a
identity value = value
