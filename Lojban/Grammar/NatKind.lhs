This module exports Nat kind and related types.

> {-# OPTIONS -XKindSignatures -XDataKinds -XGADTs -XStandaloneDeriving #-}

> module Lojban.Grammar.NatKind
> (
>   Nat(..), HNat(..),
>   Nat1, Nat2, Nat3, Nat4, Nat5,
>   zeroN, oneN, twoN, threeN, fourN, fiveN,
> ) where

Kind [of] magic:

> data Nat = Z | Su Nat
> type Nat1 = Su Z
> type Nat2 = Su Nat1
> type Nat3 = Su Nat2
> type Nat4 = Su Nat3
> type Nat5 = Su Nat4

> data HNat :: Nat -> * where
>   HZ :: HNat Z
>   HS :: HNat n -> HNat (Su n)
> deriving instance Eq (HNat (n :: Nat))
> zeroN = HZ
> oneN = HS zeroN
> twoN = HS oneN
> threeN = HS twoN
> fourN = HS threeN
> fiveN = HS fourN

