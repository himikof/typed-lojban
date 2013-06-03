This module exports Nat kind and related types.

> {-# OPTIONS -XKindSignatures -XDataKinds -XPolyKinds -XTypeFamilies -XTypeOperators #-}
> {-# OPTIONS -XUndecidableInstances -XFlexibleInstances #-}

> module Lojban.Grammar.NatKind
> (
>   Nat(..), 
>   KindIs, KindOf, KindParam,
>   Sing, SingI(..), SingE(..),
>   (+)(..), Max(..), If'(..), (<=)(..),
> )
> where

> import GHC.TypeLits

> type KindParam = KindIs ()

> type family Max (m :: Nat) (n :: Nat) :: Nat
> type instance Max m n = If' (m <=? n) n m

> type family If' (c :: Bool) (a :: k) (b :: k) :: k
> type instance If' True a b = a
> type instance If' False a b = b

> {-
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

> -}
