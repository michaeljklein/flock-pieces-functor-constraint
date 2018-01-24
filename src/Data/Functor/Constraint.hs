
module Data.Functor.Constraint
  ( ConstC(..)

  , Jop(..)
  , mkJop
  , extractJopL
  , extractJopR

  , WrapC(..)
  , wmap

  , WrapE(..)
  , WrapP(..)
  , pmap
  ) where

import Data.Functor.Constraint.Const
import Data.Functor.Constraint.Jop
import Data.Functor.Constraint.Wrap

