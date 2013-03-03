This module exports basic TextTree definitions.

> {-# OPTIONS -XDefaultSignatures -XFlexibleInstances -XFlexibleContexts #-}
> {-# OPTIONS -XTypeOperators #-}

> module Lojban.Grammar.TextTree
> (
>   Word, TreeT(..), TextTree,
>   Textful(..),
>   mkTNode, showTextTree, reprTextTree
> ) where

> import Prelude hiding (elem)
> import GHC.Exts (IsString(..))
> import GHC.Generics
> import Data.Foldable
> import Data.Char
> import Data.List (intercalate)

> newtype Word = Word { unWord :: String } deriving (Show, Eq)
> instance IsString Word where
>   fromString = fixedWord

TextTree type is something similar to S-expressions. It can represent
hierarchical structure of text, but is untyped.

> data TreeT w = TLeaf w | TNode [TreeT w]
> type TextTree = TreeT Word
> instance Foldable TreeT where
>   foldMap f (TLeaf w) = f w
>   foldMap f (TNode l) = foldMap (foldMap f) l
> instance Show (TextTree) where
>   show = showTextTree
> isEmptyTT :: TreeT w -> Bool
> isEmptyTT (TLeaf _) = False
> isEmptyTT (TNode l) = null l
> mkTNode :: [TreeT w] -> TreeT w
> mkTNode l = TNode $ filter (not . isEmptyTT) l

> flatten :: TextTree -> [Word]
> flatten = foldMap (\x -> [x])

> vowels :: String
> vowels = "aeiou"
> isVowel :: Char -> Bool
> isVowel c = (c `elem` vowels)

> fixedWord :: String -> Word
> fixedWord "" = Word ""
> fixedWord (x:xs) = Word $ prefix ++ xl : xs where
>   xl = toLower x
>   prefix | isVowel xl = "."
>          | otherwise = ""

Textful class contains all typed representations of TextTrees.

> class Eq t => Textful t where
>   untype :: t -> TextTree
>   default untype :: (Generic t, GPhraseUntype (Rep t)) => t -> TextTree
>   untype = guntype . from

Generic phrase untyping:

> class GPhraseUntype f where
>   guntype :: f a -> TextTree
>   guntype = mkTNode . guntype2
>   guntype2 :: f a -> [TextTree]
>   guntype2 x = case guntype x of
>       u@(TLeaf _) -> [u]
>       TNode l -> l
> constructorAsPhrase :: Constructor c => (C1 c t) a -> TextTree
> constructorAsPhrase = TLeaf . fixedWord . conName
> instance (Constructor c) => GPhraseUntype (C1 c U1)  where
>   guntype = constructorAsPhrase
>   --guntype2 = undefined
> instance (Constructor c, GPhraseUntype t) =>
>       GPhraseUntype (C1 c (S1 NoSelector t)) where
>   guntype x = mkTNode $ [constructorAsPhrase x] ++
>       guntype2 (unM1 $ unM1 x)
> --  guntype2 = undefined
> instance GPhraseUntype (Rec0 Word) where
>   guntype (K1 w) = TLeaf w
>   guntype2 (K1 w) = [TLeaf w]
> instance (Generic p, GPhraseUntype (Rep p)) => GPhraseUntype (Par0 p) where
>   guntype (K1 p) = guntype $ from p
>   guntype2 (K1 p) = guntype2 $ from p
> {-instance (GPhraseUntype a, GPhraseUntype b) => GPhraseUntype (a :*: b) where
>   guntype (L1 x) = guntype x -}
> instance (GPhraseUntype a, GPhraseUntype b) => GPhraseUntype (a :+: b) where
>   guntype2 (L1 x) = guntype2 x
>   guntype2 (R1 x) = guntype2 x
>   guntype (L1 x) = guntype x
>   guntype (R1 x) = guntype x
> instance (Datatype d, GPhraseUntype f) => GPhraseUntype (D1 d f) where
>   guntype2 (M1 x) = guntype2 x
>   guntype (M1 x) = guntype x

> showTextTree :: TextTree -> String
> showTextTree = (intercalate " ") . (map unWord) . flatten
> reprTextTree :: TextTree -> String
> reprTextTree (TLeaf w) = unWord w
> reprTextTree (TNode l) = "(" ++ intercalate " " (map reprTextTree l) ++ ")"
