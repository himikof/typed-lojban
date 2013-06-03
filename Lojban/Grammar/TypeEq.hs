{-# OPTIONS -XPolyKinds -XKindSignatures -XDataKinds -XScopedTypeVariables #-}
{-# OPTIONS -XExplicitNamespaces -XTypeOperators -XGADTs #-}
{-# OPTIONS -XConstraintKinds -XTypeFamilies #-}

module Lojban.Grammar.TypeEq
(
    type (:~:) (..),
    EqT(..), eqT1,
    liftEq, liftEq2, coerce, coerce1,
    lower, lower2,
    liftPlus,
    CtxDict (..),
)
where

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Maybe (isJust)

class EqT (f :: k -> *) where
    type EqTCtx f a :: Constraint
    type EqTCtx f a = ()
    eqT0 :: {-(EqTCtx f a, EqTCtx f b) => -}f a -> f b -> Maybe (a :~: b)

instance EqT ((:~:) a) where
    eqT0 Refl Refl = Just Refl

liftEq :: a :~: b -> f a :~: f b
liftEq Refl = Refl

liftEq2 :: a :~: b -> c :~: d -> f a c :~: f b d
liftEq2 Refl Refl = Refl

coerce :: a :~: b -> a -> b
coerce Refl = id

coerce1 :: a :~: b -> f a -> f b
coerce1 Refl = id

lower :: (f a :~: f b) -> a :~: b
lower Refl = Refl

lower2 :: (f a c :~: f b d) -> (a :~: b, c :~: d)
lower2 Refl = (Refl, Refl)

eqT1 :: (Eq (f b), EqT f{-, EqTCtx f a, EqTCtx f b-}) => f a -> f b -> Bool
eqT1 x y = case x `eqT0` y of
    Just proof -> coerce1 proof x == y
    Nothing -> False

-- Helpers

liftPlus :: a :~: b -> c :~: d -> (a + c) :~: (b + d)
liftPlus Refl Refl = Refl

data CtxDict ctx where
  CtxDict :: ctx => CtxDict ctx
