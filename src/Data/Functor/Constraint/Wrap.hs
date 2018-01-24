{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Functor.Constraint.Wrap where

import Data.Proxy (Proxy)


-- | A wrapper type where the first argument is a given constraint on the value.
data WrapC c a where
  WrapC :: c a => a -> WrapC c a

instance Show a => Show (WrapC Show a) where
  show (WrapC x) = show x


-- | No `Functor` instance for `WrapC`, the constraints won't allow it
wmap :: c b => (a -> b) -> WrapC c a -> WrapC c b
wmap f (WrapC x) = WrapC (f x)


-- | Variant of @c a => a -> _ c a@, apply constraint differently
--
-- @
-- resolveE :: P c (E c) -> P c (E ())
-- @
--
data WrapE c a where
  WrapE :: c (WrapE c) => a -> WrapE c a

instance Functor (WrapE c) where
  fmap f (WrapE x) = WrapE (f x)


-- | Variant of @c a => a -> _ c a@, no value
--
-- @
-- resolveE :: P c (E c) -> P c (E ())
--
--
-- • Could not deduce: c b arising from a use of ‘P’
--   from the context: c a
--     bound by a pattern with constructor:
--                P :: forall (c :: * -> Constraint) a. c a => Proxy a -> P c a,
--              in an equation for ‘fmap’
--     at /Users/michaelklein/Desktop/flock/pieces/through/src/Scratch/Hs4.hs:1458:11-13
-- • In the expression: P (fmap f x)
--   In an equation for ‘fmap’: fmap f (P x) = P (fmap f x)
--   In the instance declaration for ‘Functor (P c)’
--
-- instance Functor (P c) where
--   fmap f (P x) = P (fmap f x)
-- @
--
data WrapP c a where
  WrapP :: c a => Proxy a -> WrapP c a

-- | No `Functor` instance for `WrapP`, the constraints won't allow it
pmap :: c b => (a -> b) -> WrapP c a -> WrapP c b
pmap f (WrapP x) = WrapP (fmap f x)


