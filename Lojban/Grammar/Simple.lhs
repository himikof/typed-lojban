Простое представление грамматики.

> {-# OPTIONS -XDeriveGeneric -XOverloadedStrings #-}

> module Lojban.Grammar.Simple
> (
>   Selbri(..), Sumti(..),
>   SimpleSumti(..),
>   module Lojban.Grammar.Basic,
> ) where

> import Lojban.Grammar.TextTree
> import Lojban.Grammar.NatKind
> import Lojban.Grammar.TypeEq
> import Lojban.Grammar.Basic
> import GHC.Generics

> data Sumti = Sumti Quantifier SimpleSumti
> instance Textful Sumti where
>   untype (Sumti EmptyQ s) = untype s
>   untype (Sumti q s) = mkTNode (liftedUntype (q, s)) [] []

> data SimpleSumti
>   = La Word
>   | ProSumti KOhA
>   | LESumti LE LESumtiCtx Selbri
> instance Textful SimpleSumti where
>   untype (La w) = TLeaf w
>   untype (ProSumti ps) = untype ps
>   untype (LESumti d c s) = mkTNode [] [untype d] $ liftedUntype (s, mkKu c)

> data Selbri 
>   = Brivla Word
>   | Tanru TanruOp TanruOpCtx Selbri Selbri
>   | NASelbri Selbri
> instance Textful Selbri where
>   untype (Brivla w) = TLeaf w
>   untype (Tanru op ctx l r) = mkTNode [untype $ mkKe ctx, ul] [untype op]
>                                       [ur, untype $ mkKe'e ctx] where
>       (ul, ur) = untypeArgsOrdered op l r
>       untypeArgsOrdered Co l r = (untype r, untype l)
>       untypeArgsOrdered _ l r = (untype l, untype r)


> data LESumtiCtx = LESumtiCtx { hasKu :: Bool }
> defaultLEC :: LESumtiCtx
> defaultLEC = LESumtiCtx { hasKu = False }
> mkKu :: LESumtiCtx -> Elidable KU
> mkKu c = if hasKu c then Just Ku else Nothing


> data KeState = HasKEAndKEhE | HasKE | HasNone
> data TanruOpCtx = TanruOpCtx { keState :: KeState }
> defaultTC :: TanruOpCtx
> defaultTC = TanruOpCtx { keState = HasNone }

> mkKe :: TanruOpCtx -> Elidable KE
> mkKe c = case keState c of
>   HasNone -> Nothing
>   _       -> Just Ke
> mkKe'e :: TanruOpCtx -> Elidable KEhE
> mkKe'e c = case keState c of
>   HasKEAndKEhE -> Just Ke'e
>   _            -> Nothing


> data TanruOp = GroupApp | Bo | Co deriving (Eq, Generic)
> instance Textful TanruOp where
>   untype GroupApp = emptyTNode
>   untype Bo = TLeaf "bo"
>   untype Co = TLeaf "co"

