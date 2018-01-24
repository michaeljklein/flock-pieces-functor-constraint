{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Constraint.Const where

import Data.Void (Void)


-- | Pass `Void` to @c@, guaranteeing the first arg is unused
newtype ConstC c a = ConstC { getConstC :: c Void a }

-- | Extract first
instance Eq (c Void a) => Eq (ConstC c a) where
  ConstC x == ConstC y = x == y

-- | Extract first
instance Ord (c Void a) => Ord (ConstC c a) where
  compare (ConstC x) (ConstC y) = compare x y

-- | Extract first
instance Show (c Void a) => Show (ConstC c a) where
  showsPrec n = showsPrec n . getConstC

-- | Map inside
instance Functor (c Void) => Functor (ConstC c) where
  fmap f (ConstC x) = ConstC (fmap f x)


