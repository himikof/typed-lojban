Базовые грамматические определения.

> {-# OPTIONS -XDeriveGeneric -XDeriveDataTypeable -XOverloadedStrings #-}

> module Lojban.Grammar.Basic
> (
>   Elidable,
>   NA(..), KOhA(..), 
>   KE(..), KEhE(..),
>   LE(..), KU(..),
>   UI(..), CAI(..),
>   NAI(..), Quantifier(..),
>   CU(..),
> ) where

> import Lojban.Grammar.TextTree
> import GHC.Generics
> import Data.Typeable

Elidable functor:

> type Elidable = Maybe
> instance Textful t => Textful (Maybe t) where
>   untype = maybe emptyTNode untype

> data NA = Na deriving (Eq, Generic)
> instance Textful NA where

> data CU = Cu deriving (Eq, Generic)
> instance Textful CU where

> data KE = Ke deriving (Eq, Generic)
> instance Textful KE where
> data KEhE = Ke'e deriving (Eq, Generic)
> instance Textful KEhE where

> data KU = Ku deriving (Eq, Generic)
> instance Textful KU where

> data LE = Le | Lo deriving (Eq, Generic)
> instance Textful LE where

> data KOhA = Mi | Do | Ti | Ta | Tu | Zo'e
>   deriving (Eq, Generic, Typeable)
> instance Textful KOhA where

> data Quantifier = EmptyQ | Ro | Su'o deriving (Eq, Generic)
> instance Textful Quantifier where

> data UI = Ui deriving (Eq, Generic, Typeable)
> instance Textful UI where
> --instance FreeGrammarTag UI where

> data CAI = Cai|Cu'i|Pei|Sai|Ne'e deriving (Eq, Generic, Typeable)
> instance Textful CAI where
> --instance FreeGrammarTag CAI where

> data NAI = Nai deriving (Eq, Generic, Typeable)
> instance Textful NAI where 
> --instance FreeGrammarTag NAI where
