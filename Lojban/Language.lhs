This module exports lojban words and constructions.

> {-# OPTIONS -XTypeFamilies -XDeriveGeneric -XDeriveDataTypeable -XOverloadedStrings #-}

> module Lojban.Language
> (
>   ProSumti(..), Cmene(..),
>   tavla, pelxu, dunda, zdani,
>   UI(..), CAI(..), NAI(..)
> ) where

> import Lojban.Grammar
> import Lojban.Grammar.NatKind
> import GHC.Generics hiding (moduleName)
> import Data.Typeable

Some pro-sumti (pronouns and such):

> data ProSumti = Mi | Do | Ti | Ta | Tu | Zo'e
>   deriving (Eq, Generic, Typeable)
> instance Textful ProSumti where
> instance FGTaggable ProSumti where
>   type FGTagged ProSumti = SumtiFGT
>   withFGTagC = SumtiFGT
> instance Sumti ProSumti where

Defining cmene (names):

> data Cmene = La Word deriving (Eq, Generic, Typeable)
> instance Textful Cmene where
> instance FGTaggable Cmene where
>   type FGTagged Cmene = SumtiFGT
>   withFGTagC = SumtiFGT
> instance Sumti Cmene where

Example brivla:

> --- x1 (talker) talks to x2 (audience) about x3 (topic) in language x4
> tavla :: Brivla Nat4
> tavla = Brivla fourN "tavla"

> --- x1 is yellow
> pelxu :: Brivla Nat1
> pelxu = Brivla oneN "pelxu"

> --- x1 gives x2 to x3 (without payment)
> dunda :: Brivla Nat3
> dunda = Brivla threeN "dunda"

> --- x1 is a nest/house/lair/den/[home] of/for x2
> zdani :: Brivla Nat2
> zdani = Brivla twoN "zdani"

Attitudinals implementation:

> data UI = Ui deriving (Eq, Generic, Typeable)
> instance Textful UI where
> instance FreeGrammarTag UI where

> data CAI = Cai|Cu'i|Pei|Sai|Ne'e deriving (Eq, Generic, Typeable)
> instance Textful CAI where
> instance FreeGrammarTag CAI where

Universal negator NAI:
It can be used as a FreeGrammarTag, among other places.

> data NAI = Nai deriving (Eq, Generic, Typeable)
> instance Textful NAI where 
> instance FreeGrammarTag NAI where
